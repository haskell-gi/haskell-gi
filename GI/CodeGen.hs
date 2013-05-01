{-# LANGUAGE RecordWildCards, NamedFieldPuns, OverloadedStrings #-}

module GI.CodeGen
    ( genConstant
    , genFunction
    , genCode
    , genModule
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM, forM_, when)
import Control.Monad.Writer (tell)
import Data.Char (toLower, toUpper)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Tuple (swap)
import Data.Typeable (TypeRep, tyConName, typeRepTyCon)
import qualified Data.Map as M

import GI.API
import GI.Code
import GI.GObject
import GI.Type
import GI.Util (split)
import GI.Value
import GI.Internal.ArgInfo
import GI.Internal.FunctionInfo

valueStr VVoid         = "()"
valueStr (VBoolean x)  = show x
valueStr (VInt8 x)     = show x
valueStr (VUInt8 x)    = show x
valueStr (VInt16 x)    = show x
valueStr (VUInt16 x)   = show x
valueStr (VInt32 x)    = show x
valueStr (VUInt32 x)   = show x
valueStr (VInt64 x)    = show x
valueStr (VUInt64 x)   = show x
valueStr (VFloat x)    = show x
valueStr (VDouble x)   = show x
valueStr (VGType x)    = show x
valueStr (VUTF8 x)     = show x
valueStr (VFileName x) = show x

interfaceClassName = (++"Iface_")

toPtr con = "(\\(" ++ con ++ " x) -> x)"

padTo n s = s ++ replicate (n - length s) ' '

escapeReserved "type" = "type_"
escapeReserved "in" = "in_"
escapeReserved "data" = "data_"
escapeReserved "instance" = "instance_"
escapeReserved "where" = "where_"
-- Reserved because we generate code that uses this name.
escapeReserved "result" = "result_"
escapeReserved s = s

lcFirst (x:xs) = toLower x : xs
lcFirst "" = error "lcFirst: empty string"

ucFirst (x:xs) = toUpper x : xs
ucFirst "" = error "ucFirst: empty string"

getPrefix :: String -> CodeGen String
getPrefix ns = do
    cfg <- config
    case M.lookup ns (prefixes cfg) of
        Just p -> return p
        Nothing -> return ns

-- Return a qualified prefix for the given namespace. In case the
-- namespace corresponds to the current module the empty string is returned.
qualify :: String -> CodeGen String
qualify ns = do
     cfg <- config
     return $ if modName cfg == ns then
                ""
              else
                ucFirst ns ++ "."

-- Returns whether the given type corresponds to a ManagedPtr
-- instance, currently we manage only APIObjects.
isManaged   :: Type -> CodeGen Bool
isManaged t = do
  a <- findAPI t
  case a of
    Just (APIObject _) -> return True
    _                  -> return False

specifiedName s fallback = do
    cfg <- config

    case M.lookup s (names cfg) of
        Just s' -> return s'
        Nothing -> fallback

literalName (Name ns s) = specifiedName s lit
    where lit = do
              prefix <- getPrefix ns
              return $ lcFirst prefix ++ "_" ++ s

lowerName (Name _ s) = specifiedName s lowered
    where lowered = return $ concat . rename $ split '_' s

          rename [w] = [lcFirst w]
          rename (w:ws) = lcFirst w : map ucFirst' ws
          rename [] = error "rename: empty list"

          ucFirst' "" = "_"
          ucFirst' x = ucFirst x

upperName (Name ns s) = do
          name <- specifiedName s uppered
          prefix <- qualify ns
          return $ prefix ++ name
    where uppered = return $ concatMap ucFirst' $ split '_' $ sanitize s
          -- Move leading underscores to the end (for example in
          -- GObject::_Value_Data_Union -> GObject::Value_Data_Union_)
          sanitize ('_':xs) = sanitize xs ++ "_"
          sanitize xs = xs

          ucFirst' "" = "_"
          ucFirst' x = ucFirst x

mapPrefixes :: Type -> CodeGen Type
mapPrefixes t@(TBasicType _) = return t
mapPrefixes (TArray t) = TArray <$> mapPrefixes t
mapPrefixes (TGList t) = TGList <$> mapPrefixes t
mapPrefixes (TGSList t) = TGSList <$> mapPrefixes t
mapPrefixes (TGHash ta tb) =
  TGHash <$> mapPrefixes ta <*> mapPrefixes tb
mapPrefixes t@TError = return t
mapPrefixes (TInterface ns s) = do
    -- We qualify symbols with their namespace, unless they are in the
    -- current module.
    prefix <- qualify ns
    return $ TInterface undefined $ prefix ++ s

haskellType' :: Type -> CodeGen TypeRep
haskellType' t = haskellType <$> mapPrefixes t

foreignType' :: Type -> CodeGen TypeRep
foreignType' t = do
  isScalar <- getIsScalar
  if isScalar
     -- Enum and flag values are represented by machine words.
    then return $ "Word" `con` []
    else foreignType <$> mapPrefixes t

  where getIsScalar = do
          a <- findAPI t
          case a of
            Nothing -> return False
            (Just (APIEnum _)) -> return True
            (Just (APIFlags _)) -> return True
            _ -> return False

prime = (++ "'")

genConstant :: Name -> Constant -> CodeGen ()
genConstant n@(Name _ name) (Constant value) = do
    name' <- literalName n
    ht <- haskellType' $ valueType value
    line $ "-- constant " ++ name
    line $ name' ++ " :: " ++ show ht
    line $ name' ++ " = " ++ valueStr value

mkForeignImport :: String -> Callable -> CodeGen ()
mkForeignImport symbol callable = foreignImport $ do
    line first
    indent $ do
        mapM_ (\a -> line =<< fArgStr a) (args callable)
        line =<< last
    where
    first = "foreign import ccall \"" ++ symbol ++ "\" " ++
                symbol ++ " :: "
    fArgStr arg = do
        ft <- foreignType' $ argType arg
        let ft' = case direction arg of
              DirectionInout -> ptr ft
              DirectionOut -> ptr ft
              DirectionIn -> ft
        let start = show ft' ++ " -> "
        return $ padTo 40 start ++ "-- " ++ argName arg
    last = show <$> io <$> foreignType' (returnType callable)

data Expr = Var String
          | App String Expr
          | M Expr
          | EStr String String
          deriving Show

data Do = Bind String Expr | Let String Expr

-- This is somewhat ugly.
code :: Expr -> (String, [String])
code e = case doExpr e of
             ds@((Bind s _) : _) -> (s, map doStr $ reverse ds)
             ds@((Let s _) : _) -> (s, map doStr $ reverse ds)
             [] -> (name e, [])
  where
    name :: Expr -> String
    name (Var s) = s
    name (App _ e) = prime $ name e
    name (M e) = name e
    name (EStr n _) = n

    doExpr :: Expr -> [Do]
    doExpr (Var _) = []
    doExpr (App f e) = Let (prime $ name e) (App f (Var $ name e)) : (doExpr e)
    doExpr (M (App f e)) = Bind (prime $ name e) (App f (Var $ name e)) : (doExpr e)
    doExpr (M (EStr n e)) = [Bind n (EStr n e)]
    doExpr _ = error "doExpr"

    doStr (Let s e) = "let " ++ s ++ " = " ++ exprStr e
    doStr (Bind s e) = s ++ " <- " ++ exprStr e

    exprStr (Var s) = s
    exprStr (App f e) = f ++ " " ++ exprStr e
    exprStr (EStr _ s) = s
    exprStr _ = error "exprStr"

interfaceName :: Type -> CodeGen (Maybe Name)
interfaceName t = do
    a <- findAPI t
    case a of
         Just (APIInterface _) -> do
              case t of
                   TInterface ns n -> return $ Just $ Name ns n
                   _ -> error "Interface without TInterface!"
         _ -> return Nothing

hToF :: Type -> CodeGen (Expr -> Expr)
hToF t = do
    hType <- haskellType' t
    fType <- foreignType' t
    if hType == fType
      then return id
      else do
        conv <- convertGeneratedType
        case conv of
          Just c -> return c
          Nothing ->
            if ptr hType == fType
               then
                 let con = tyConName $ typeRepTyCon hType
                 in return $ App $ toPtr con
               else
                 return $ hToF' (show hType) (show fType)

    where
    convertGeneratedType = do
       a <- findAPI t
       case a of
         Just (APIEnum _) -> do
           return $ Just $ App "(fromIntegral . fromEnum)"
         Just (APIFlags _) -> do
           return $ Just $ App "fromIntegral"
         Just (APIObject _) -> do
           return $ Just $ App "(castPtr . unsafeManagedPtrGetPtr)"
         Just (APIInterface _) -> do
           return $ Just $ App "(castPtr . unsafeManagedPtrGetPtr)"
         _ -> return Nothing
    hToF' "[Char]" "CString" = M . App "newCString"
    hToF' "Word"   "Type"   = App "fromIntegral"
    hToF' "Bool"   "CInt"    = App "(fromIntegral . fromEnum)"
    hToF' "[Char]" "Ptr CString" = M . App "bullshit"
    hToF' hType fType = error $
        "don't know how to convert " ++ hType ++ " to " ++ fType

fToH :: Type -> CodeGen (Expr -> Expr)
fToH t = do
    hType <- haskellType' t
    fType <- foreignType' t
    if hType == fType
      then return id
      else do
        conv <- convertGeneratedType
        case conv of
          Just f -> return f
          Nothing ->
            if ptr hType == fType
               then do
                    let constructor = tyConName $ typeRepTyCon hType
                    isGO <- isGObject t
                    prefixGO <- qualify "GObject"
                    if isGO
                       then return $ M . App (prefixGO ++ "makeNewObject "
                                              ++ constructor)
                       else do
                          --- These are for routines that return
                          --- abstract interfaces. We create a managed
                          --- pointer without actual refcounting.
                          ifaceName <- interfaceName t
                          case ifaceName of
                               Just _ -> do
                                    return $ M . App (constructor ++ " <$> newForeignPtr_")
                               _ -> return $ App $ constructor
               else return $ fToH' (show fType) (show hType)

    where
    convertGeneratedType = do
       a <- findAPI t
       case a of
         Just (APIEnum _) -> do
           return $ Just $ App "(toEnum . fromIntegral)"
         Just (APIFlags _) -> do
           return $ Just $ App "fromIntegral"
         -- XXX: Do we ever need to convert out-arguments that are
         -- specified by an interface type?
         _ -> return Nothing
    fToH' "CString" "[Char]" = M . App "peekCString"
    fToH' "Type" "Word" = App "fromIntegral"
    fToH' "CInt" "Bool" = App "(/= 0)"
    fToH' fType hType = error $
       "don't know how to convert " ++ fType ++ " to " ++ hType

convert :: Expr -> CodeGen (Expr -> Expr) -> CodeGen String
convert e f = do
  converter <- f
  let (name, lines) = code $ converter e
  mapM_ line lines
  return name

hOutType callable outArgs = do
    hReturnType <- haskellType' $ returnType callable
    hOutArgTypes <- mapM (haskellType' . argType) outArgs
    let justType = case outArgs of
            [] -> hReturnType
            _ -> "(,)" `con` (hReturnType : hOutArgTypes)
        maybeType = "Maybe" `con` [justType]
    return $ if returnMayBeNull callable then maybeType else justType

genCallable :: Name -> String -> Callable -> CodeGen ()
genCallable n symbol callable = do
    mkForeignImport symbol callable
    wrapper

    where
    inArgs = filter ((== DirectionIn) . direction) $ args callable
    outArgs = filter ((== DirectionOut) . direction) $ args callable
    wrapper = group $ do
        let argName' = escapeReserved . argName
        name <- lowerName n
        signature
        line $
            name ++ " " ++
            intercalate " " (map argName' inArgs) ++
            " = do"
        indent $ do
            argNames <- convertIn
            line $ "result <- " ++ symbol ++ concatMap (" " ++) argNames
            touchInArgs
            convertOut
    signature = do
        name <- lowerName n
        line $ name ++ " ::"
        ifs <- inArgInterfaces
        let jIfs = catMaybes ifs
        indent $ do
            when (not $ null jIfs) $ do
                let ifaceList = map (\(c, ifaces) ->
                                         map (++ (' ':[c])) ifaces) jIfs
                line $ "(" ++ intercalate ", " (concat ifaceList) ++ ") =>"
            forM_ (zip ifs inArgs) $ \(mIface, a) ->
                case mIface of
                    Just (c, _) -> line $ withComment ([c] ++ " ->") $ argName a
                    Nothing -> line =<< hArgStr a
            result >>= line
    inArgInterfaces :: CodeGen [Maybe (Char, [[Char]])]
    inArgInterfaces = rec "abcdefghijklmnopqrtstuvwxyz" inArgs
        where rec [] _ = error "out of letters"
              rec _ [] = return []
              rec (c:cs) (arg:args) = do
                  api <- findAPI $ argType arg
                  case api of
                      Just (APIInterface _) -> do
                          s <- show <$> (haskellType' $ argType arg)
                          rest <- rec cs args
                          return $ Just (c, [interfaceClassName s,
                                             "ManagedPtr"]) : rest
                      -- Instead of restricting to the actual class,
                      -- we allow for any object descending from it.
                      Just (APIObject _) -> do
                        isGO <- isGObject $ argType arg
                        case isGO of
                           True -> do
                             s <- show <$> (haskellType' $ argType arg)
                             rest <- rec cs args
                             return $ Just (c, [klass s, "ManagedPtr"]) : rest
                           False -> (Nothing :) <$> rec (c:cs) args
                      _ -> (Nothing :) <$> rec (c:cs) args
    convertIn = forM (args callable) $ \arg -> do
        ft <- foreignType' $ argType arg
        let name = escapeReserved $ argName arg
        if direction arg == DirectionIn
            then convert (Var name) (hToF $ argType arg)
            else convert (M $ EStr name $ "malloc :: " ++ show (io $ ptr ft)) (return id)
    -- XXX: Should create ForeignPtrs for pointer results.
    -- XXX: Check argument transfer.
    convertOut = do
        -- Convert return value and out paramters.
        result <- convert (Var "result") (fToH $ returnType callable)
        pps <- forM outArgs $ \arg -> do
            convert (M $ App "peek" (Var $ escapeReserved $ argName arg))
              (fToH $ argType arg)
        out <- hOutType callable outArgs
        case (show out, outArgs) of
            ("()", []) -> line $ "return ()"
            ("()", _) -> line $ "return (" ++ intercalate ", " pps ++ ")"
            (_ , []) -> line $ "return " ++ result
            (_ , _) -> line $
                "return (" ++ intercalate ", " (result : pps) ++ ")"
    -- Touch in arguments so we are sure that they exist when the C
    -- function was called.
    touchInArgs = forM_ (args callable) $ \arg -> do
        when (direction arg == DirectionIn) $ do
             managed <- isManaged (argType arg)
             when managed $ do
               let name = escapeReserved $ argName arg
               line $ "touchManagedPtr " ++ name
    withComment a b = padTo 40 a ++ "-- " ++ b
    hArgStr arg = do
        ht <- haskellType' $ argType arg
        let start = show ht ++ " -> "
        return $ withComment start $ argName arg
    result = show <$> io <$> hOutType callable outArgs

genFunction :: Name -> Function -> CodeGen ()
genFunction n (Function symbol callable _flags) = do
  line $ "-- function " ++ symbol
  genCallable n symbol callable

genStruct :: Name -> Struct -> CodeGen ()
genStruct n@(Name _ name) (Struct _fields) = do
  line $ "-- struct " ++ name
  name' <- upperName n
  line $ "data " ++ name' ++ " = " ++ name' ++ " (Ptr " ++ name' ++ ")"
  -- XXX: Generate code for fields.

genEnum :: Name -> Enumeration -> CodeGen ()
genEnum n@(Name ns name) (Enumeration fields) = do
  line $ "-- enum " ++ name
  name' <- upperName n
  fields' <- forM fields $ \(fieldName, value) -> do
      n <- upperName $ Name ns (name ++ "_" ++ fieldName)
      return (n, value)
  group $ do
    line $ "data " ++ name' ++ " = "
    indent $ do
      case fields' of
        ((fieldName, _value):fs) -> do
          line $ "  " ++ fieldName
          forM_ fs $ \(n, _) -> line $ "| " ++ n
        _ -> return()
  group $ do
    line $ "instance Enum " ++ name' ++ " where"
    indent $ forM_ fields' $ \(n, v) ->
      line $ "fromEnum " ++ n ++ " = " ++ show v
    let valueNames = M.toList . M.fromListWith (curry snd) $ map swap fields'
    blank
    indent $ forM_ valueNames $ \(v, n) ->
      line $ "toEnum " ++ show v ++ " = " ++ n

genFlags :: Name -> Flags -> CodeGen ()
genFlags n@(Name _ name) (Flags (Enumeration _fields)) = do
  line $ "-- flags " ++ name
  name' <- upperName n
  -- XXX: Generate code for fields.
  -- XXX: We should generate code for converting to/from lists.
  line $ "type " ++ name' ++ " = Word"

genCallback :: Name -> Callback -> CodeGen ()
genCallback n _ = do
  name' <- upperName n
  line $ "-- callback " ++ name' ++ " "
  -- XXX
  --line $ "data " ++ name' ++ " = " ++ name' ++ " (Ptr (IO ()))"
  line $ "data " ++ name' ++ " = " ++ name' ++ " (Ptr " ++ name' ++ ")"

genUnion n _ = do
  name' <- upperName n
  line $ "-- union " ++ name' ++ " "
  -- XXX
  line $ "data " ++ name' ++ " = " ++ name' ++ " (Ptr " ++ name' ++ ")"

genMethod :: Name -> Name -> Function -> CodeGen ()
genMethod cn mn (Function {
                    fnSymbol = sym,
                    fnCallable = c,
                    fnFlags = fs }) = do
    name' <- upperName cn
    returnsGObject <- isGObject (returnType c)
    line $ "-- method " ++ name' ++ "::" ++ (name mn)
    let -- Mangle the name to namespace it to the class.
        mn' = mn { name = name cn ++ "_" ++ name mn }
        -- Mangle the callable to make the implicit object parameter
        -- explicit.
        c' = c {  args = args' }
        args' = objArg : (args c)
        objArg = Arg {
          argName = "_obj",
          argType = TInterface (namespace cn) (name cn),
          direction = DirectionIn,
          mayBeNull = False,
          scope = ScopeTypeInvalid,
          transfer = TransferNothing }
    let -- Make GObject-derived constructors return the actual type of
        -- the object.
        c'' = c { returnType = returnType' }
        returnType' = if returnsGObject then
                        TInterface (namespace cn) (name cn)
                      else
                        returnType c        
    if FunctionIsConstructor `elem` fs
      then genCallable mn' sym c''
      else genCallable mn' sym c'

-- The marshaller C code has some built in support for basic types, so
-- we only generate conversions for things that the marshaller cannot
-- do itself. (This list should be kept in sync with hsgclosure.c)

-- Marshaller to haskell types.
-- There is no support in the marshaller for converting Haskell
-- strings into C strings directly.
marshallFType :: Type -> CodeGen TypeRep
marshallFType t@(TBasicType TUTF8) = foreignType' t
marshallFType t@(TBasicType TFileName) = foreignType' t
marshallFType t@(TBasicType _) = return $ haskellType t
marshallFType a = foreignType' a

convertFMarshall name t@(TBasicType TUTF8) = convert (Var name) (fToH t)
convertFMarshall name t@(TBasicType TFileName) = convert (Var name) (fToH t)
convertFMarshall name (TBasicType _) = return name
convertFMarshall name t = convert (Var name) (fToH t)

convertHMarshall name t@(TBasicType TUTF8) = convert (Var name) (hToF t)
convertHMarshall name t@(TBasicType TFileName) = convert (Var name) (hToF t)
convertHMarshall name (TBasicType _) = return name
convertHMarshall name t = convert (Var name) (hToF t)

genSignal :: Name -> Signal -> Name -> Object -> CodeGen ()
genSignal sn (Signal { sigCallable = cb }) on _o = do
  on' <- upperName on
  let (w:ws) = split '-' $ name sn
      sn' = w ++ concatMap ucFirst ws
  line $ "-- signal " ++ on' ++ "::" ++ name sn

  let inArgs = filter ((== DirectionIn) . direction) $ args cb
      outArgs = filter ((== DirectionOut) . direction) $ args cb

  -- Callback prototype
  let cbType = on' ++ ucFirst sn' ++ "Callback"
  group $ do
    line $ "type " ++ cbType ++ " = "
    indent $ do
      -- gtk2hs does not pass the object to the callback, we follow
      -- the same conventions.
      --  t <- haskellType' $ TInterface
      -- (namespace on) (name on) line $ show t ++ " ->"
      forM_ inArgs $ \arg -> do
        ht <- haskellType' $ argType arg
        line $ show ht ++ " ->"
      ret <- io <$> hOutType cb outArgs
      line $ show ret

  -- Wrapper for connecting functions to the signal
  prefixGO <- qualify "GObject"
  let signature = "(ManagedPtr a, " ++ klass (prefixGO ++ "Object") ++ " a) =>"
                  ++ " a -> " ++ cbType ++ " -> IO Word32" 
  -- XXX It would be better to walk through the whole tree and
  -- disambiguate only those that are ambiguous.
  let signalConnectorName = "on" ++ on' ++ ucFirst sn'
  group $ do
    line $ signalConnectorName ++ " :: " ++ signature
    line $ signalConnectorName ++ " obj cb = " ++ prefixGO ++
           "connectSignal obj \"" ++ (name sn) ++ "\" cb' where"
    indent $ do
        line $ "cb' :: Ptr " ++ prefixGO ++ "Object ->"
        indent $ do forM_ (args cb) $ \arg -> do
                       ft <- marshallFType $ argType arg
                       line $ show ft ++ " ->"
                    ret <- io <$> (marshallFType $ returnType cb)
                    line $ show ret
        let inArgNames = map (escapeReserved . argName) inArgs
            outArgNames = map (escapeReserved . argName) outArgs
            allNames = map (escapeReserved . argName) (args cb)
        line $ "cb' _ " ++ (concatMap (++ " ") allNames) ++ "= do"
        indent $ do
{-                   when ((not . null) outArgs) $
                     line $ "out: " ++ (show outArgs) -}
                   inNames <- forM (zip inArgNames inArgs) $ \(name, arg) ->
                        convertFMarshall name (argType arg)
                   let hRetval = case outArgs of
                           [] -> "ret"
                           _ -> "(" ++ intercalate ", " ("ret":outArgNames) ++ ")" 
                   line $ hRetval ++ " <- cb" ++ (concatMap (" " ++) inNames)
{-
                   -- XXX non-basic type out values are not written back yet.

                   forM_ (zip outArgNames outArgs) $ \(name, arg) ->
                      case argType arg of
                        TBasicType t -> line $ "poke " ++ 
                      n' <- convert (Var name) (hToF $ argType arg)
                   -}
                   retval <- convertHMarshall "ret" (returnType cb)
                   line $ "return " ++ retval

-- Instantiation mechanism, so we can convert different object types
-- descending from GObject into each other.
genGObjectType iT n = do
  name' <- upperName n
  let className = klass name'

  -- ManagedPtr implementation
  group $ do
    line $ "instance ManagedPtr " ++ name' ++ " where"
    indent $ do
            line $ "unsafeManagedPtrGetPtr = (\\(" ++ name' ++
                     " x) -> unsafeForeignPtrToPtr x)"
            line $ "touchManagedPtr        = (\\(" ++ name' ++
                     " x) -> touchForeignPtr x)"


  group $ line $ "class " ++ className ++ " o"

  group $ do
    line $ "instance " ++ className ++ " " ++ name'
    forM_ iT $ \ancestor -> do
          ancestor' <- upperName ancestor
          line $ "instance " ++ (klass ancestor') ++ " " ++ name'

-- Type casting with type checking
genGObjectCasts n o = do
  name' <- upperName n

  let cn_ = objTypeInit o

  group $ do
    line $ "foreign import ccall unsafe \"" ++ cn_ ++ "\""
    indent $ line $ "c_" ++ cn_ ++ " :: Type"

  prefixGO <- qualify "GObject"

  group $ do
    line $ "castTo" ++ name' ++ " :: " ++
           "(ManagedPtr o, " ++ klass (prefixGO ++ "Object") ++ " o) => " ++
           "o -> IO " ++ name'
    line $ "castTo" ++ name' ++ " = " ++ prefixGO ++ "castTo " ++ name' ++ 
           " c_" ++ cn_ ++ " \"" ++ name' ++ "\""

-- We do not currently manage properly APIObjects not descending from
-- GObjects, but we should do so eventually. For the moment we just
-- implement no-ops here.
manageUnManagedPtr n = do
  name' <- upperName n
  group $ do
    line $ "instance ManagedPtr " ++ name' ++ " where"
    indent $ do
            line $ "unsafeManagedPtrGetPtr = (\\(" ++ name' ++ " x) -> x)"
            line $ "touchManagedPtr      _ = return ()"

genObject n o = do
  name' <- upperName n

  line $ "-- object " ++ name' ++ " "

  let t = (\(Name ns' n') -> TInterface ns' n') n
  isGO <- isGObject t

  when (not isGO) $ line $ "-- XXX APIObject \"" ++ name' ++
           "\" does not descend from GObject."

  line $ "newtype " ++ name' ++ " = " ++ name' ++
       if isGO then
          " (ForeignPtr " ++ name' ++ ")"
       else
          " (Ptr " ++ name' ++ ")"
  cfg <- config

  -- Instances and type conversions
  if isGO
  then genGObjectType (instanceTree (instances cfg) n) n
  else manageUnManagedPtr n

  -- Implemented interfaces
  let oIfs = objInterfaces o
  when ((not . null) oIfs) $ group $ forM_ oIfs $ \(Name ns n) -> do
    prefix <- qualify ns
    let ifClass = prefix ++ interfaceClassName n
    line $ "instance " ++ ifClass ++ " " ++ name'

  -- Type safe casting
  when isGO $ genGObjectCasts n o

  -- Methods
  forM_ (objMethods o) $ \(mn, f) -> do
    genMethod n mn f

  -- And finally signals
  forM_ (objSignals o) $ \(sn, s) -> genSignal sn s n o

genInterface n iface = do
  -- For each interface, we generate a class IFoo and a data structure
  -- Foo. We only really need a separate Foo so that we can return
  -- them from bound functions. In principle we might be able to do
  -- something more elegant with existential types.

  name' <- upperName n
  let cls = interfaceClassName name'
  line $ "-- interface " ++ name' ++ " "
  line $ "newtype " ++ name' ++ " = " ++ name' ++ " (ForeignPtr " ++ name' ++ ")"
  line $ "class " ++ cls ++ " a"
  line $ "instance " ++ cls ++ " " ++ name'
  forM_ (ifMethods iface) $ \(mn, f) -> do
    -- Some type libraries seem to include spurious interface methods,
    -- where a method Mod.Foo::func is actually just a function
    -- mod_foo_func that doesn't take the interface as an (implicit)
    -- first argument. If we find a matching function, we don't
    -- generate the method.
    others_ <- others (fnSymbol f)
    when (not others_) $ genMethod n mn f

   where -- It may be expedient to keep a map of symbol -> function.
         others sym = do
              cfg <- config
              return $ any (hasSymbol sym . snd) $ M.toList $ input cfg

         hasSymbol sym1 (APIFunction (Function { fnSymbol = sym2 })) = sym1 == sym2
         hasSymbol _ _ = False

genCode :: Name -> API -> CodeGen ()
genCode n (APIConst c) = genConstant n c
genCode n (APIFunction f) = genFunction n f
genCode n (APIEnum e) = genEnum n e
genCode n (APIFlags f) = genFlags n f
genCode n (APICallback c) = genCallback n c
genCode n (APIStruct s) = genStruct n s
genCode n (APIUnion u) = genUnion n u
genCode n (APIObject o) = genObject n o
genCode n (APIInterface i) = genInterface n i
genCode _ (APIBoxed _) = return ()

gLibBootstrap = do
    line "type Type = Word"
    line "data GArray a = GArray (Ptr (GArray a))"
    line "data GHashTable a b = GHashTable (Ptr (GHashTable a b))"
    line "data GList a = GList (Ptr (GList a))"
    line "data GSList a = GSList (Ptr (GSList a))"
    blank

gObjectBootstrap = do
    line "-- Reference counting for constructors"
    line "foreign import ccall unsafe \"&g_object_unref\""
    indent $ line "ptr_to_g_object_unref :: FunPtr (Ptr a -> IO ())"
    blank
    line "makeNewObject :: (ForeignPtr a -> a) -> Ptr b -> IO a"
    line "makeNewObject constructor ptr = do"
    indent $ do
           line "_ <- g_object_ref_sink $ castPtr ptr"
           line "fPtr <- newForeignPtr ptr_to_g_object_unref $ castPtr ptr"
           line "return $! constructor fPtr"
    blank
    line "-- Safe casting machinery"
    line "foreign import ccall unsafe \"check_object_type\""
    line "    c_check_object_type :: Ptr Object -> Type -> CInt"
    blank
    line $ "castTo :: (ManagedPtr o, " ++ (klass "Object") ++ " o, "
           ++ (klass "Object") ++ " o') =>"
    line "           (ForeignPtr o' -> o') -> Type -> [Char] -> o -> IO o'"
    line "castTo constructor t typeName obj = do"
    line "    let ptrObj = (castPtr . unsafeManagedPtrGetPtr) obj"
    line "    when (c_check_object_type ptrObj t /= 1) $"
    line "         error $ \"Cannot cast object to \" ++ typeName"
    line "    result <- makeNewObject constructor ptrObj"
    line "    touchManagedPtr obj"
    line "    return result"
    blank
    line "-- Connecting GObjects to signals"
    line "foreign import ccall unsafe \"gtk2hs_closure_new\""
    line "  gtk2hs_closure_new :: StablePtr a -> IO (Ptr Closure)"
    blank
    line "foreign import ccall \"g_signal_connect_closure\" g_signal_connect_closure' ::"
    line "    Ptr Object ->                           -- instance"
    line "    CString ->                              -- detailed_signal"
    line "    Ptr Closure ->                          -- closure"
    line "    CInt ->                                 -- after"
    line "    IO Word32"
    blank
    line "connectSignal :: (ObjectKlass o, ManagedPtr o) => "
    line "                  o -> [Char] -> a -> IO Word32"
    line "connectSignal object signal fn = do"
    line "      closure <- newStablePtr fn >>= gtk2hs_closure_new"
    line "      signal' <- newCString signal"
    line "      let object' = (castPtr . unsafeManagedPtrGetPtr) object"
    line "      result <- g_signal_connect_closure' object' signal' closure 0"
    line "      touchManagedPtr object"
    line "      return result"

genModule :: String -> [(Name, API)] -> CodeGen ()
genModule name apis = do
    line $ "-- Generated code."
    blank
    line $ "{-# LANGUAGE ForeignFunctionInterface #-}"
    blank
    -- XXX: This should be a command line option.
    let installationPrefix = "GI."
        ip = (installationPrefix ++)
    -- XXX: Generate export list.
    line $ "module " ++ ip (ucFirst name) ++ " where"
    blank
    -- String and IOError also appear in GLib.
    line $ "import Prelude hiding (String, IOError)"
    -- Basic data types for bindings.
    when (name /= "GLib") $
         line $ "import " ++ ip "GLib (GArray(..), GList(..), "
                ++ "GSList(..), GHashTable(..), Error(..), Type(..))"
    line $ "import Data.Int"
    line $ "import Data.Word"
    line $ "import Foreign.Safe"
    line $ "import Foreign.ForeignPtr.Unsafe"
    line $ "import Foreign.C"
    line $ "import Control.Applicative ((<$>))"
    line $ "import Control.Monad (when)"
    blank
    line $ "import GI.Internal.ManagedPtr"
    blank
    cfg <- config

    let code = codeToList $ runCodeGen' cfg $
          forM_ (filter (not . (`elem` ignore) . GI.API.name . fst) apis)
          (uncurry genCode)
    -- GLib bootstrapping hacks.
    let code' = case name of
          "GLib" -> (runCodeGen' cfg $ gLibBootstrap) : code
          "GObject" -> (runCodeGen' cfg $ gObjectBootstrap) : code
          _ -> code
    forM_ (imports cfg) $ \i -> do
      line $ "import qualified " ++ ip (ucFirst i) ++ " as " ++ ucFirst i
    blank
    mapM_ (\c -> tell c >> blank) code'

    where ignore = [
            "dummy_decl",
            -- These API elements refer to symbols which are
            -- dynamically loaded, which ghci has trouble with. Skip
            -- them.
            "IOModule",
            "io_modules_load_all_in_directory",
            "io_modules_load_all_in_directory_with_scope",
            "signal_set_va_marshaller"]
