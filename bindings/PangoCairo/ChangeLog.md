### 1.0.12
Fix [#77](https://github.com/haskell-gi/haskell-gi/issues/77),
	changes the return value of
	[fontMapGetDefault](https://hackage.haskell.org/package/gi-pangocairo/docs/GI-PangoCairo-Interfaces-FontMap.html#v:fontMapGetDefault),
	[fontMapNew](https://hackage.haskell.org/package/gi-pangocairo/docs/GI-PangoCairo-Interfaces-FontMap.html#v:fontMapNew)
	and
	[fontMapNewForFontType](https://hackage.haskell.org/package/gi-pangocairo/docs/GI-PangoCairo-Interfaces-FontMap.html#v:fontMapNewForFontType)
	from
	[Pango.FontMap](https://hackage.haskell.org/package/gi-pango/docs/GI-Pango-Objects-FontMap.html#t:FontMap)
	to
	[PangoCairo.FontMap](https://hackage.haskell.org/package/gi-pangocairo/docs/GI-PangoCairo-Interfaces-FontMap.html#t:FontMap).
