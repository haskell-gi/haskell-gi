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
import Data.Char (toLower, toUpper, isUpper)
import Data.List (intercalate, partition)
import Data.Maybe (catMaybes)
import Data.Tuple (swap)
import qualified Data.ByteString.Char8 as B
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
          cfg <- config
          name <- specifiedName s uppered
          return $ if ns == modName cfg then
                      name
                   else
                      (ucFirst ns) ++ "." ++ name
    where uppered = return $ concatMap ucFirst' $ split '_' $ sanitize s
          -- Move leading underscores to the end (for example in
          -- GObject::_Value_Data_Union -> GObject::Value_Data_Union_)
          sanitize ('_':xs) = sanitize xs ++ "_"
          sanitize xs = xs

          ucFirst' "" = "_"
          ucFirst' x = ucFirst x

-- Given a CamelCase string "SpinButton" convert to lowercase with
-- underscores: "spin_button".
-- Special cases are things like HBox, HSV, IMContext, i.e. a string
-- of uppercase letters and then an actual type, we take care of those
-- with groupSingles.
camelToUnderscores c = B.unpack $ groupSingles $ B.pack $
                        flip concatMap c $ \char ->
                            if isUpper char then
                                '_' : (toLower char) : []
                            else
                                char : []

-- Group 1-letter groups.
-- Example: a_b_c_def -> abc_def
groupSingles :: B.ByteString -> B.ByteString
groupSingles t = reconstruct $ joinOneLetterClusters $
                             discardFirst $ B.split '_' t
                where
                -- Ignore the effect of the first empty underscore 
                discardFirst ("":xs) = xs
                discardFirst _ = error $ "Parse error 1 on " ++ (B.unpack t)

                reconstruct [] = error $ "Parse error 2 on " ++ (B.unpack t)
                reconstruct all@(x:xs) =
                      -- Sometimes there is a single letter in the
                      -- beginning, join with the first chunk in this
                      -- case: "VBox" -> _v_box -> vbox
                      if B.length x == 1 then
                         B.append x (B.intercalate "_" xs)
                      else
                         case xs of
                              -- HSV -> _h_s_v -> hsv
                              [] -> x
                              -- IMContext -> _i_m_context -> im_context
                              -- or simply Label -> _label -> label
                              _ -> B.intercalate "_" all

-- Join together neighboring 1-letter clusters, leaving the rest of
-- clusters untouched.
joinOneLetterClusters :: [B.ByteString] -> [B.ByteString]
joinOneLetterClusters chunks = filter (not . B.null) $ reverse $ go "" [] chunks
        where go acc1 result [] = acc1 : result
              go acc1 result (x:xs) =
                 if B.length x < 2 then
                    go (B.append acc1 x) result xs
                 else
                    go "" (x : acc1 : result) xs

-- Name in C conventions.
cName (Name ns n) = do
      prefix <- map toLower <$> getPrefix ns
      return $ prefix ++ "_" ++ (camelToUnderscores n)

mapPrefixes :: Type -> CodeGen Type
mapPrefixes t@(TBasicType _) = return t
mapPrefixes (TArray t) = TArray <$> mapPrefixes t
mapPrefixes (TGList t) = TGList <$> mapPrefixes t
mapPrefixes (TGSList t) = TGSList <$> mapPrefixes t
mapPrefixes (TGHash ta tb) =
  TGHash <$> mapPrefixes ta <*> mapPrefixes tb
mapPrefixes t@TError = return t
mapPrefixes (TInterface ns s) = do
    -- We qualify symbols with their namespace.
    return $ TInterface undefined $ (ucFirst ns) ++ "." ++ s

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
           isGO <- isGObject t
           let toType :: Type -> String
               toType (TInterface ns n) = (ucFirst ns) ++ ".to" ++ n
               toType _ = error "We expected a TInterface!"
               unType :: Type -> String
               unType (TInterface ns n) = (ucFirst ns) ++ ".un" ++ n
               unType _ = error "We expected a TInterface!"
           return $ if isGO then
                       Just $ App $ "unsafeForeignPtrToPtr $ " ++
                                    unType t ++ " $ " ++ toType t
                    else
                       Nothing
         Just (APIInterface _) -> do
           -- When an argument is an instance of an interface, use
           -- that interface's class's conversion function.
           t' <- haskellType' t
           case t of
             TInterface ns n ->
               return $ Just $ M . App ( "unsafeForeignPtrToPtr <$> "
                                         ++ toPtr (show t') ++ " <$> "
                                         ++ ns ++ ".to" ++ n )
             _ -> error "Interface does not have interface type!?"
         _ -> return Nothing
    hToF' "[Char]" "CString" = M . App "newCString"
    hToF' "Word"   "GLib.Type"   = App "fromIntegral"
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
                    if isGO
                       then return $ M . App ("GObject.makeNewObject " ++ constructor)
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
    fToH' "GLib.Type" "Word" = App "fromIntegral"
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
                line $ "(" ++ (
                  intercalate ", " $
                  map (\(c, iface) ->
                        iface ++ " " ++ [c])
                  jIfs) ++
                  ") =>"
            forM_ (zip ifs inArgs) $ \(mIface, a) ->
                case mIface of
                    Just (c, _iface) -> line $ withComment ([c] ++ " ->") $ argName a
                    Nothing -> line =<< hArgStr a
            result >>= line
    inArgInterfaces :: CodeGen [Maybe (Char, [Char])]
    inArgInterfaces = rec "abcdefghijklmnopqrtstuvwxyz" inArgs
        where rec [] _ = error "out of letters"
              rec _ [] = return []
              rec (c:cs) (arg:args) = do
                  api <- findAPI $ argType arg
                  case api of
                      Just (APIInterface _) -> do
                          s <- show <$> (haskellType' $ argType arg)
                          rest <- rec cs args
                          return $ Just (c, interfaceClassName s) : rest
                      -- Instead of restricting to the actual class,
                      -- we allow for any object descending from it.
                      Just (APIObject _) -> do
                        isGO <- isGObject $ argType arg
                        case isGO of
                           True -> do
                             s <- show <$> (haskellType' $ argType arg)
                             rest <- rec cs args
                             return $ Just (c, klass s) : rest
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
             let name = escapeReserved $ argName arg
             isGO <- isGObject (argType arg)
             when isGO $ do
                  line $ "touchForeignPtr $ (GObject.unObject . GObject.toObject) $ " ++ name
             iName <- interfaceName (argType arg)
             case iName of
                  Just (Name ns n) -> do
                       let unInterface = "(\\(" ++ (ucFirst ns) ++ "." ++
                                         n ++ " x) -> x)"
                       let toInterface = (ucFirst ns) ++ ".to" ++ n
                       line $ unInterface ++ " <$> " ++ toInterface ++ " " ++
                              name ++ " >>= touchForeignPtr"
                  _ -> return ()
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
  let signature = (klass "GObject.Object") ++ " a => a -> " ++ cbType ++ " -> IO Word32" 
  -- XXX It would be better to walk through the whole tree and
  -- disambiguate only those that are ambiguous.
  let signalConnectorName = "on" ++ on' ++ ucFirst sn'
  group $ do
    line $ signalConnectorName ++ " :: " ++ signature
    line $ signalConnectorName ++ " obj cb = GObject.connectSignal obj \"" ++
           (name sn) ++ "\" cb' where"
    indent $ do
        line $ "cb' :: Ptr GObject.Object ->"
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
genGObjectDescendantConversions iT n = do
  name' <- upperName n
  let className = klass name'
  parent <- upperName $ head iT

  -- Get the raw pointer
  line $ "un" ++ name' ++ " (" ++ name' ++ " o) = o"

  group $ do
    line $ "class " ++ (klass parent) ++ " o => " ++ className ++ " o"
    line $ "to" ++ name' ++ " :: " ++ className ++
           " o => o -> " ++ name'
    line $ "to" ++ name' ++ " = GObject.unsafeCastObject . GObject.toObject"

  group $ do
    line $ "instance " ++ className ++ " " ++ name' ++ " where"
    forM_ iT $ \ancestor -> do
          ancestor' <- upperName ancestor
          line $ "instance " ++ (klass ancestor') ++ " " ++ name'
               ++ " where"
    indent $ do
      line $ "toObject = GObject.Object . castForeignPtr . un" ++ name'
      line $ "unsafeCastObject = " ++ name' ++
             " . castForeignPtr . GObject.unObject"

  -- Type casting with type checking
  cn_ <- (++ "_get_type") <$> cName n
  group $ do
    line $ "foreign import ccall unsafe \"" ++ cn_ ++ "\""
    indent $ line $ "c_" ++ cn_ ++ " :: GLib.Type"

  group $ do
    line $ "castTo" ++ name' ++ " :: " ++ (klass "GObject.Object") ++ " o"
           ++ " => o -> IO " ++ name'
    line $ "castTo" ++ name' ++ " = GObject.castTo " ++ name' ++ 
           " c_" ++ cn_ ++ " \"" ++ name' ++ "\""

genGObjectConversions n = do
  name' <- upperName n
  let className = klass name'

  -- Get the raw pointer
  line $ "un" ++ name' ++ " (" ++ name' ++ " o) = o"

  group $ do
        line $ "class " ++ className ++ " o where"
        indent $ do
               line $ "to" ++ name' ++ " :: o -> " ++ name'
               line $ "unsafeCast" ++ name' ++ " :: " ++ name' ++ " -> o"
  group $ do
        line $ "instance " ++ className ++ " " ++ name' ++ " where"
        indent $ do
               line $ "to" ++ name' ++ " = id"
               line $ "unsafeCast" ++ name' ++ " = id"

  group $ do
    line $ "castTo" ++ name' ++ " :: " ++ className ++ " o => o -> IO o"
    line $ "castTo" ++ name' ++ " = return"

genObject n o = do
  name' <- upperName n
  line $ "-- object " ++ name' ++ " "

  let t = (\(Name ns' n') -> TInterface ns' n') n
  isGO <- isGObject t

  line $ "newtype " ++ name' ++ " = " ++ name' ++
       if isGO then
          " (ForeignPtr " ++ name' ++ ")"
       else
          " (Ptr " ++ name' ++ ")"
  cfg <- config

  when isGO $ do
       let iT = instanceTree (instances cfg) n
       case iT of
            [] -> genGObjectConversions n
            _  -> genGObjectDescendantConversions iT n

  -- Implemented interfaces.
  forM_ (objInterfaces o) $ \(Name ns n) -> do
    let toIfName = "to" ++ n
    let ifClass = (ucFirst ns) ++ "." ++ interfaceClassName n
    let tyName = (ucFirst ns) ++ "." ++ n
    let cast = if isGO then
                  "withForeignPtr p $ \\p' -> GObject.makeNewObject " ++
                  tyName ++ " p'"
               else
                  tyName ++ " <$> newForeignPtr_ (castPtr p)"
    group $ do
      line $ "instance " ++ ifClass ++ " " ++ name' ++ " where"
      indent $ line $ toIfName ++ " (" ++ name' ++ " p) = " ++ cast

  -- Methods.
  forM_ (objMethods o) $ \(mn, f) -> do
    genMethod n mn f

  -- And finally signals.
  forM_ (objSignals o) $ \(sn, s) -> genSignal sn s n o

genInterface n iface = do
  -- For each interface, we generate a class IFoo and a data structure
  -- Foo. We only really need a separate Foo so that we can return
  -- them from bound functions. In principle we might be able to do
  -- something more elegant with existential types.

  name' <- upperName n
  let cls = interfaceClassName name'
  line $ "-- interface " ++ name' ++ " "
  -- XXX
  line $ "newtype " ++ name' ++ " = " ++ name' ++ " (ForeignPtr " ++ name' ++ ")"
  line $ "class " ++ cls ++ " a where"
  indent $ do
    line $ "to" ++ name' ++ " :: a -> IO " ++ name'
  line $ "instance " ++ cls ++ " " ++ name' ++ " where"
  indent $ do
    line $ "to" ++ name' ++ " = return"
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
    line "    c_check_object_type :: Ptr GObject.Object -> GLib.Type -> CInt"
    blank
    line $ "castTo :: (" ++ (klass "GObject.Object") ++ " o, "
           ++ (klass "GObject.Object") ++ " o') =>"
    line "           (ForeignPtr o' -> o') -> GLib.Type -> [Char] -> o -> IO o'"
    line "castTo constructor t typeName obj ="
    line "    let ptrObj = (unsafeForeignPtrToPtr . unObject . toObject) obj in"
    line "        if c_check_object_type ptrObj t == 1 then"
    line "              withForeignPtr ((unObject . toObject) obj) $ \\ptr -> "
    line "                makeNewObject constructor ptr"
    line "          else"
    line "              error $ \"Cannot cast object to \" ++ typeName"
    blank
    line "-- Connecting GObjects to signals"
    line "foreign import ccall unsafe \"gtk2hs_closure_new\""
    line "  gtk2hs_closure_new :: StablePtr a -> IO (Ptr GObject.Closure)"
    blank
    line "foreign import ccall \"g_signal_connect_closure\" g_signal_connect_closure' ::"
    line "    Ptr GObject.Object ->                   -- instance"
    line "    CString ->                              -- detailed_signal"
    line "    Ptr GObject.Closure ->                  -- closure"
    line "    CInt ->                                 -- after"
    line "    IO Word32"
    blank
    line "connectSignal :: GObject.ObjectKlass o => o -> [Char] -> a -> IO Word32"
    line "connectSignal object signal fn = do"
    line "      closure <- newStablePtr fn >>= gtk2hs_closure_new"
    line "      signal' <- newCString signal"
    line "      withForeignPtr (GObject.unObject (GObject.toObject object)) $ \\object' ->"
    line "          g_signal_connect_closure' object' signal' closure 0"

genModule :: String -> [(Name, API)] -> CodeGen ()
genModule name apis = do
    line $ "-- Generated code."
    blank
    line $ "{-# LANGUAGE ForeignFunctionInterface #-}"
    blank
    -- XXX: Generate export list.
    line $ "module " ++ ucFirst name ++ " where"
    blank
    -- String and IOError also appear in GLib.
    when (name == "GLib") $
         line $ "import Prelude hiding (String, IOError)"
    line $ "import Data.Int"
    line $ "import Data.Word"
    line $ "import Foreign.Safe"
    line $ "import Foreign.ForeignPtr.Unsafe"
    line $ "import Foreign.C"
    line $ "import Control.Applicative ((<$>))"
    blank
    cfg <- config

    let (foreignImports, rest_) =
          splitImports $ runCodeGen' cfg $
          forM_ (filter (not . (`elem` ignore) . GI.API.name . fst) apis)
          (uncurry genCode)
    -- GLib bootstrapping hacks.
    let rest = case name of
          "GLib" -> (runCodeGen' cfg $ gLibBootstrap) : rest_
          "GObject" -> (runCodeGen' cfg $ gObjectBootstrap) : rest_
          _ -> rest_
    forM_ (imports cfg) $ \i -> do
      line $ "import qualified " ++ ucFirst i ++ " as " ++ ucFirst i
    blank
    mapM_ (\c -> tell c >> blank) foreignImports
    mapM_ (\c -> tell c >> blank) rest

    where splitImports = partition isImport . codeToList
          isImport (ForeignImport _code) = True
          isImport _ = False

          ignore = [
            "dummy_decl",
            -- These API elements refer to symbols which are
            -- dynamically loaded, which ghci has trouble with. Skip
            -- them.
            "IOModule",
            "io_modules_load_all_in_directory",
            "io_modules_load_all_in_directory_with_scope",
            "signal_set_va_marshaller"]
