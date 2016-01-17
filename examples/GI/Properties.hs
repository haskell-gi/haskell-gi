{-# LANGUAGE DataKinds #-}

module GI.Properties
    ( _aLink
    , _abbr
    , _aboveChild
    , _accelClosure
    , _accelGroup
    , _accelKey
    , _accelMode
    , _accelMods
    , _accelPath
    , _accelWidget
    , _accept
    , _acceptCharset
    , _acceptFocus
    , _acceptsTab
    , _accessKey
    , _accumulativeMargin
    , _action
    , _actionGroup
    , _actionName
    , _actionTarget
    , _activatable
    , _activateOnSingleClick
    , _activatesDefault
    , _active
    , _activeCues
    , _activeElement
    , _activeId
    , _activeWindow
    , _addTearoffs
    , _adjustment
    , _align
    , _alignSet
    , _alignWidget
    , _alignment
    , _alinkColor
    , _allowAsync
    , _allowsEval
    , _allowsInlineScript
    , _allowsInlineStyle
    , _alpha
    , _alt
    , _altGraphKey
    , _altKey
    , _alternateTitle
    , _alwaysShowImage
    , _ancestorOrigins
    , _anchorNode
    , _anchorOffset
    , _anchors
    , _angle
    , _appCodeName
    , _appMenu
    , _appName
    , _appPaintable
    , _appVersion
    , _applets
    , _application
    , _applicationCache
    , _applyAuthorStyles
    , _archive
    , _area
    , _areas
    , _arrowType
    , _artists
    , _async
    , _attachWidget
    , _attachedTo
    , _attributes
    , _audioTracks
    , _authors
    , _autoLoadImages
    , _autoRender
    , _autoResizeWindow
    , _autoShrinkImages
    , _autocapitalize
    , _autocomplete
    , _autocorrect
    , _autofocus
    , _autoplay
    , _availHeight
    , _availLeft
    , _availTop
    , _availWidth
    , _availableHeight
    , _availableWidth
    , _axis
    , _background
    , _backgroundFullHeight
    , _backgroundFullHeightSet
    , _backgroundGdk
    , _backgroundIcon
    , _backgroundIconName
    , _backgroundRgba
    , _backgroundSet
    , _badInput
    , _baseNode
    , _baseOffset
    , _baseUri
    , _baselinePosition
    , _baselineRow
    , _behavior
    , _bgColor
    , _body
    , _booleanValue
    , _border
    , _borderWidth
    , _bottomPadding
    , _bubbles
    , _buffer
    , _buffered
    , _button
    , _buttonSensitivity
    , _buttons
    , _canDefault
    , _canFocus
    , _cancelBubble
    , _cancelButton
    , _cancelable
    , _capsLockWarning
    , _caption
    , _capture
    , _cellArea
    , _cellAreaContext
    , _cellBackground
    , _cellBackgroundGdk
    , _cellBackgroundRgba
    , _cellBackgroundSet
    , _cellIndex
    , _cellPadding
    , _cellSpacing
    , _cells
    , _centered
    , _ch
    , _chOff
    , _challenge
    , _charCode
    , _characterSet
    , _charging
    , _chargingTime
    , _charset
    , _checked
    , _child
    , _childDetached
    , _childElementCount
    , _childModel
    , _childNodes
    , _childPackDirection
    , _childRevealed
    , _children
    , _cite
    , _classList
    , _className
    , _clear
    , _clickable
    , _clientHeight
    , _clientInformation
    , _clientLeft
    , _clientTop
    , _clientWidth
    , _clientX
    , _clientY
    , _climbRate
    , _closed
    , _code
    , _codeBase
    , _codeType
    , _colSpan
    , _collapsed
    , _color
    , _colorDepth
    , _colorHash
    , _colorSelection
    , _cols
    , _columnHomogeneous
    , _columnSpacing
    , _columnSpanColumn
    , _columns
    , _comments
    , _commonAncestorContainer
    , _compact
    , _compatMode
    , _complete
    , _completion
    , _compositeChild
    , _connectEnd
    , _connectStart
    , _console
    , _content
    , _contentDocument
    , _contentEditable
    , _contentType
    , _contentWindow
    , _context
    , _control
    , _controller
    , _controls
    , _cookie
    , _cookieEnabled
    , _coords
    , _copyTargetList
    , _copyright
    , _coreObject
    , _corruptedVideoFrames
    , _count
    , _createFolders
    , _creationTime
    , _crossOrigin
    , _css
    , _cssRules
    , _cssText
    , _cssValueType
    , _ctrlKey
    , _cues
    , _currentAlpha
    , _currentColor
    , _currentNode
    , _currentPage
    , _currentRgba
    , _currentScript
    , _currentSize
    , _currentSrc
    , _currentTarget
    , _currentTime
    , _currentValue
    , _cursiveFontFamily
    , _cursorPosition
    , _cursorVisible
    , _customEncoding
    , _customError
    , _customTabLabel
    , _customTitle
    , _data
    , _dateTime
    , _day
    , _declare
    , _decorated
    , _decorationLayout
    , _decorationLayoutSet
    , _defaultCharset
    , _defaultChecked
    , _defaultEncoding
    , _defaultFontFamily
    , _defaultFontSize
    , _defaultHeight
    , _defaultMonospaceFontSize
    , _defaultMuted
    , _defaultPageSetup
    , _defaultPlaybackRate
    , _defaultPrevented
    , _defaultSelected
    , _defaultStatus
    , _defaultText
    , _defaultValue
    , _defaultView
    , _defaultWidth
    , _defer
    , _delayFactor
    , _deletable
    , _deltaMode
    , _deltaX
    , _deltaY
    , _deltaZ
    , _description
    , _designMode
    , _desktopWidth
    , _destinationUri
    , _destroyWithParent
    , _detail
    , _detailHeightRows
    , _detailWidthChars
    , _deviceDpi
    , _deviceHeight
    , _devicePixelRatio
    , _deviceWidth
    , _dialog
    , _digits
    , _dir
    , _dirName
    , _direction
    , _disabled
    , _dischargingTime
    , _displayName
    , _doOverwriteConfirmation
    , _doctype
    , _document
    , _documentElement
    , _documentUri
    , _documenters
    , _domComplete
    , _domContentLoadedEventEnd
    , _domContentLoadedEventStart
    , _domInteractive
    , _domLoading
    , _domain
    , _domainLookupEnd
    , _domainLookupStart
    , _doubleBuffered
    , _download
    , _draggable
    , _draw
    , _drawAsRadio
    , _drawIndicator
    , _drawSensitive
    , _drawValue
    , _droppedVideoFrames
    , _duration
    , _editWidget
    , _editable
    , _editableSet
    , _editedCell
    , _editing
    , _editingBehavior
    , _editingCanceled
    , _elements
    , _ellipsize
    , _ellipsizeSet
    , _embedPageSetup
    , _embedded
    , _embeds
    , _enableAcceleratedCompositing
    , _enableCaretBrowsing
    , _enableDefaultContextMenu
    , _enableDeveloperExtras
    , _enableDisplayOfInsecureContent
    , _enableDnsPrefetching
    , _enableDomPaste
    , _enableFileAccessFromFileUris
    , _enableFrameFlattening
    , _enableFullscreen
    , _enableGridLines
    , _enableHtml5Database
    , _enableHtml5LocalStorage
    , _enableHyperlinkAuditing
    , _enableJavaApplet
    , _enableMediaStream
    , _enableMediasource
    , _enableOfflineWebApplicationCache
    , _enablePageCache
    , _enablePlugins
    , _enablePopup
    , _enablePrivateBrowsing
    , _enableRunningOfInsecureContent
    , _enableScripts
    , _enableSearch
    , _enableSiteSpecificQuirks
    , _enableSmoothScrolling
    , _enableSpatialNavigation
    , _enableSpellChecking
    , _enableTreeLines
    , _enableUniversalAccessFromFileUris
    , _enableWebaudio
    , _enableWebgl
    , _enableXssAuditor
    , _enabled
    , _enabledPlugin
    , _encoding
    , _enctype
    , _endContainer
    , _endOffset
    , _endTime
    , _ended
    , _enforce96Dpi
    , _entities
    , _entryTextColumn
    , _entryType
    , _error
    , _event
    , _eventPhase
    , _events
    , _exclusive
    , _expand
    , _expandEntityReferences
    , _expanded
    , _expanderColumn
    , _expectedSize
    , _exportFilename
    , _extentNode
    , _extentOffset
    , _extraWidget
    , _face
    , _fallback
    , _fallbackSet
    , _family
    , _familySet
    , _fantasyFontFamily
    , _fetchStart
    , _fgColor
    , _file
    , _filename
    , _files
    , _fillLevel
    , _filter
    , _firstChild
    , _firstElementChild
    , _firstEmptyRegionIndex
    , _fitModel
    , _fixedHeightMode
    , _fixedWidth
    , _focusCell
    , _focusNode
    , _focusOffset
    , _focusOnClick
    , _focusOnMap
    , _focusVisible
    , _followState
    , _font
    , _fontDesc
    , _fontName
    , _foreground
    , _foregroundGdk
    , _foregroundRgba
    , _foregroundSet
    , _form
    , _formAction
    , _formEnctype
    , _formMethod
    , _formNoValidate
    , _formTarget
    , _forms
    , _fraction
    , _frame
    , _frameBorder
    , _frameElement
    , _frameName
    , _frames
    , _fromElement
    , _fullContentZoom
    , _fullscreen
    , _geolocation
    , _gfile
    , _gicon
    , _gravity
    , _group
    , _groupName
    , _gtkAlternativeButtonOrder
    , _gtkAlternativeSortArrows
    , _gtkApplicationPreferDarkTheme
    , _gtkAutoMnemonics
    , _gtkButtonImages
    , _gtkCanChangeAccels
    , _gtkColorPalette
    , _gtkColorScheme
    , _gtkCursorBlink
    , _gtkCursorBlinkTime
    , _gtkCursorBlinkTimeout
    , _gtkCursorThemeName
    , _gtkCursorThemeSize
    , _gtkDecorationLayout
    , _gtkDialogsUseHeader
    , _gtkDndDragThreshold
    , _gtkDoubleClickDistance
    , _gtkDoubleClickTime
    , _gtkEnableAccels
    , _gtkEnableAnimations
    , _gtkEnableEventSounds
    , _gtkEnableInputFeedbackSounds
    , _gtkEnableMnemonics
    , _gtkEnablePrimaryPaste
    , _gtkEnableTooltips
    , _gtkEntryPasswordHintTimeout
    , _gtkEntrySelectOnFocus
    , _gtkErrorBell
    , _gtkFallbackIconTheme
    , _gtkFileChooserBackend
    , _gtkFontName
    , _gtkFontconfigTimestamp
    , _gtkIconSizes
    , _gtkIconThemeName
    , _gtkImModule
    , _gtkImPreeditStyle
    , _gtkImStatusStyle
    , _gtkKeyThemeName
    , _gtkKeynavCursorOnly
    , _gtkKeynavWrapAround
    , _gtkLabelSelectOnFocus
    , _gtkLongPressTime
    , _gtkMenuBarAccel
    , _gtkMenuBarPopupDelay
    , _gtkMenuImages
    , _gtkMenuPopdownDelay
    , _gtkMenuPopupDelay
    , _gtkModules
    , _gtkPrimaryButtonWarpsSlider
    , _gtkPrintBackends
    , _gtkPrintPreviewCommand
    , _gtkRecentFilesEnabled
    , _gtkRecentFilesLimit
    , _gtkRecentFilesMaxAge
    , _gtkScrolledWindowPlacement
    , _gtkShellShowsAppMenu
    , _gtkShellShowsDesktop
    , _gtkShellShowsMenubar
    , _gtkShowInputMethodMenu
    , _gtkShowUnicodeMenu
    , _gtkSoundThemeName
    , _gtkSplitCursor
    , _gtkThemeName
    , _gtkTimeoutExpand
    , _gtkTimeoutInitial
    , _gtkTimeoutRepeat
    , _gtkTitlebarDoubleClick
    , _gtkTitlebarMiddleClick
    , _gtkTitlebarRightClick
    , _gtkToolbarIconSize
    , _gtkToolbarStyle
    , _gtkTooltipBrowseModeTimeout
    , _gtkTooltipBrowseTimeout
    , _gtkTooltipTimeout
    , _gtkTouchscreenMode
    , _gtkVisibleFocus
    , _gtkXftAntialias
    , _gtkXftDpi
    , _gtkXftHinting
    , _gtkXftHintstyle
    , _gtkXftRgba
    , _hadjustment
    , _halign
    , _handlePosition
    , _hasAlpha
    , _hasDefault
    , _hasDepthBuffer
    , _hasEntry
    , _hasFocus
    , _hasFrame
    , _hasOpacityControl
    , _hasOrigin
    , _hasPalette
    , _hasResizeGrip
    , _hasSelection
    , _hasStencilBuffer
    , _hasSubtitle
    , _hasTooltip
    , _hasToplevelFocus
    , _hash
    , _head
    , _headerRelief
    , _headers
    , _headersClickable
    , _headersVisible
    , _heading
    , _height
    , _heightRequest
    , _helpButton
    , _hexpand
    , _hexpandSet
    , _hhomogeneous
    , _hidden
    , _hideIfEmpty
    , _hideTitlebarWhenMaximized
    , _history
    , _homogeneous
    , _horizontalScrollbarPolicy
    , _host
    , _hostname
    , _hoverExpand
    , _hoverSelection
    , _href
    , _hreflang
    , _hscrollPolicy
    , _hscrollbarPolicy
    , _hspace
    , _html5LocalStorageDatabasePath
    , _htmlFor
    , _httpEquiv
    , _icon
    , _iconName
    , _iconSet
    , _iconSize
    , _iconSizeSet
    , _iconUri
    , _iconWidget
    , _iconic
    , _icons
    , _id
    , _idColumn
    , _identifier
    , _ignoreHidden
    , _imContext
    , _imModule
    , _image
    , _imagePosition
    , _imageUri
    , _images
    , _implementation
    , _inconsistent
    , _incremental
    , _indent
    , _indentSet
    , _indeterminate
    , _index
    , _indicatorSize
    , _initialScaleFactor
    , _inlineCompletion
    , _inlineSelection
    , _innerBorder
    , _innerHeight
    , _innerHtml
    , _innerNode
    , _innerText
    , _innerWidth
    , _inputEncoding
    , _inputHints
    , _inputPurpose
    , _inspectedUri
    , _internalSubset
    , _invalidIteratorState
    , _inverted
    , _invisible
    , _invisibleChar
    , _invisibleCharSet
    , _invisibleSet
    , _isActive
    , _isCollapsed
    , _isContentEditable
    , _isExpanded
    , _isExpander
    , _isFocus
    , _isId
    , _isImportant
    , _isLocked
    , _isMap
    , _isMaximized
    , _isShowing
    , _itemOrientation
    , _itemPadding
    , _itemWidth
    , _javascriptCanAccessClipboard
    , _javascriptCanOpenWindowsAutomatically
    , _javascriptProfilingEnabled
    , _jobName
    , _jsHeapSizeLimit
    , _justification
    , _justificationSet
    , _justify
    , _keyCode
    , _keyIdentifier
    , _keyLocation
    , _keycode
    , _keytype
    , _kind
    , _kineticScrolling
    , _label
    , _labelFill
    , _labelWidget
    , _labelXalign
    , _labelYalign
    , _labels
    , _lang
    , _language
    , _languageSet
    , _lastChild
    , _lastElementChild
    , _lastModified
    , _lastVisitedTime
    , _layerX
    , _layerY
    , _layoutStyle
    , _leftGravity
    , _leftMargin
    , _leftMarginSet
    , _leftPadding
    , _length
    , _letterSpacing
    , _letterSpacingSet
    , _level
    , _levelIndentation
    , _license
    , _licenseType
    , _limit
    , _line
    , _lines
    , _link
    , _linkColor
    , _linkUri
    , _links
    , _list
    , _loadEventEnd
    , _loadEventStart
    , _loadStatus
    , _localName
    , _localOnly
    , _localStorage
    , _location
    , _locationbar
    , _locationbarVisible
    , _logo
    , _logoIconName
    , _longDesc
    , _loop
    , _lower
    , _lowerStepperSensitivity
    , _lowsrc
    , _manifest
    , _margin
    , _marginBottom
    , _marginEnd
    , _marginHeight
    , _marginLeft
    , _marginRight
    , _marginStart
    , _marginTop
    , _marginWidth
    , _markup
    , _markupColumn
    , _matches
    , _max
    , _maxChildrenPerLine
    , _maxLength
    , _maxPosition
    , _maxValue
    , _maxWidth
    , _maxWidthChars
    , _maximumScaleFactor
    , _media
    , _mediaGroup
    , _mediaPlaybackAllowsInline
    , _mediaPlaybackRequiresUserGesture
    , _mediaText
    , _mediaUri
    , _menu
    , _menuModel
    , _menuName
    , _menubar
    , _menubarVisible
    , _message
    , _messageArea
    , _messageType
    , _metaKey
    , _method
    , _mimeType
    , _mimeTypes
    , _min
    , _minChildrenPerLine
    , _minContentHeight
    , _minContentWidth
    , _minPosition
    , _minValue
    , _minWidth
    , _minimumFontSize
    , _minimumHeight
    , _minimumKeyLength
    , _minimumLogicalFontSize
    , _minimumScaleFactor
    , _minimumWidth
    , _mnemonicKeyval
    , _mnemonicWidget
    , _mnemonicsVisible
    , _modal
    , _mode
    , _model
    , _modifierMask
    , _modifierState
    , _monitor
    , _monospace
    , _monospaceFontFamily
    , _month
    , _multiple
    , _muted
    , _nColumns
    , _nPages
    , _nPagesToPrint
    , _nPoints
    , _nRows
    , _name
    , _names
    , _namespaceUri
    , _naturalHeight
    , _naturalWidth
    , _navigation
    , _navigationStart
    , _navigator
    , _networkRequest
    , _networkResponse
    , _networkState
    , _nextElementSibling
    , _nextSibling
    , _noHref
    , _noMonthChange
    , _noResize
    , _noShade
    , _noShowAll
    , _noValidate
    , _noWrap
    , _nodeName
    , _nodeType
    , _nodeValue
    , _nonce
    , _notations
    , _numberValue
    , _numeric
    , _obeyChild
    , _object
    , _offscreenBuffering
    , _offsetHeight
    , _offsetLeft
    , _offsetParent
    , _offsetTop
    , _offsetWidth
    , _offsetX
    , _offsetY
    , _okButton
    , _onLine
    , _opacity
    , _open
    , _openFlags
    , _opener
    , _options
    , _orientation
    , _origin
    , _originalUri
    , _outerHeight
    , _outerHtml
    , _outerText
    , _outerWidth
    , _overlayScrolling
    , _overset
    , _overwrite
    , _overwriteMode
    , _ownerDocument
    , _ownerElement
    , _ownerNode
    , _ownerRule
    , _packDirection
    , _page
    , _pageIncrement
    , _pageSize
    , _pageX
    , _pageXOffset
    , _pageY
    , _pageYOffset
    , _paintClock
    , _paragraphBackground
    , _paragraphBackgroundGdk
    , _paragraphBackgroundRgba
    , _paragraphBackgroundSet
    , _parent
    , _parentElement
    , _parentNode
    , _parentRule
    , _parentStyleSheet
    , _pasteTargetList
    , _path
    , _pathname
    , _pattern
    , _patternMismatch
    , _pauseOnExit
    , _paused
    , _performance
    , _permission
    , _personalbar
    , _ping
    , _pixbuf
    , _pixbufAnimation
    , _pixbufColumn
    , _pixbufExpanderClosed
    , _pixbufExpanderOpen
    , _pixelDepth
    , _pixelSize
    , _pixelsAboveLines
    , _pixelsAboveLinesSet
    , _pixelsBelowLines
    , _pixelsBelowLinesSet
    , _pixelsInsideWrap
    , _pixelsInsideWrapSet
    , _placeholder
    , _placeholderText
    , _platform
    , _playbackRate
    , _playbackState
    , _played
    , _plugins
    , _pointerBeforeReferenceNode
    , _pointingTo
    , _popover
    , _populateAll
    , _popup
    , _popupCompletion
    , _popupFixedWidth
    , _popupSetWidth
    , _popupShown
    , _popupSingleMatch
    , _port
    , _position
    , _positionSet
    , _poster
    , _preferredStylesheetSet
    , _prefix
    , _preload
    , _previewText
    , _previewWidget
    , _previewWidgetActive
    , _previousElementSibling
    , _previousSibling
    , _primaryIconActivatable
    , _primaryIconGicon
    , _primaryIconName
    , _primaryIconPixbuf
    , _primaryIconSensitive
    , _primaryIconStock
    , _primaryIconStorageType
    , _primaryIconTooltipMarkup
    , _primaryIconTooltipText
    , _printBackgrounds
    , _printSettings
    , _product
    , _productSub
    , _profile
    , _programName
    , _progress
    , _progressFraction
    , _progressPulseStep
    , _propagationPhase
    , _protocol
    , _publicId
    , _pulse
    , _pulseStep
    , _radio
    , _rangeCount
    , _rangeOverflow
    , _rangeUnderflow
    , _ratio
    , _readOnly
    , _readyState
    , _reason
    , _receivesDefault
    , _recentManager
    , _redirectCount
    , _redirectEnd
    , _redirectStart
    , _referenceNode
    , _referrer
    , _registerSession
    , _rel
    , _relatedAction
    , _relatedTarget
    , _relativeTo
    , _relief
    , _renderer
    , _reorderable
    , _reportUrIs
    , _requestStart
    , _required
    , _reserveToggleSize
    , _resetStyleInheritance
    , _resizable
    , _resizableTextAreas
    , _resizeGripVisible
    , _resizeMode
    , _resizeToplevel
    , _resource
    , _respectImageOrientation
    , _responseEnd
    , _responseStart
    , _restrictToFillLevel
    , _resultType
    , _returnValue
    , _rev
    , _revealChild
    , _reversed
    , _rgba
    , _rightJustified
    , _rightMargin
    , _rightMarginSet
    , _rightPadding
    , _rise
    , _riseSet
    , _role
    , _root
    , _roundDigits
    , _rowHomogeneous
    , _rowIndex
    , _rowSpacing
    , _rowSpan
    , _rowSpanColumn
    , _rows
    , _rubberBanding
    , _rules
    , _rulesHint
    , _sandbox
    , _sansSerifFontFamily
    , _scale
    , _scaleFactor
    , _scaleSet
    , _scheme
    , _scope
    , _screen
    , _screenLeft
    , _screenTop
    , _screenX
    , _screenY
    , _scripts
    , _scrollAmount
    , _scrollDelay
    , _scrollHeight
    , _scrollLeft
    , _scrollOffset
    , _scrollTop
    , _scrollWidth
    , _scrollX
    , _scrollY
    , _scrollable
    , _scrollbarVisible
    , _scrollbars
    , _scrolling
    , _seamless
    , _search
    , _searchColumn
    , _searchMode
    , _searchModeEnabled
    , _secondaryIconActivatable
    , _secondaryIconGicon
    , _secondaryIconName
    , _secondaryIconPixbuf
    , _secondaryIconSensitive
    , _secondaryIconStock
    , _secondaryIconStorageType
    , _secondaryIconTooltipMarkup
    , _secondaryIconTooltipText
    , _secondaryText
    , _secondaryUseMarkup
    , _sectionRowIndex
    , _secureConnectionStart
    , _securityOrigin
    , _securityPolicy
    , _seekable
    , _seeking
    , _selectMultiple
    , _selectable
    , _selected
    , _selectedFiles
    , _selectedIndex
    , _selectedOptions
    , _selectedStylesheetSet
    , _selectionBound
    , _selectionDirection
    , _selectionEnd
    , _selectionMode
    , _selectionStart
    , _self
    , _selfScrolling
    , _sensitive
    , _serifFontFamily
    , _sessionStorage
    , _settings
    , _shadowType
    , _shape
    , _sheet
    , _shiftKey
    , _shortLabel
    , _showAll
    , _showArrow
    , _showBorder
    , _showCloseButton
    , _showConnectToServer
    , _showDayNames
    , _showDefault
    , _showDefaultItem
    , _showDesktop
    , _showDetails
    , _showDialogItem
    , _showEditor
    , _showEnterLocation
    , _showExpanders
    , _showFallback
    , _showFillLevel
    , _showHeading
    , _showHidden
    , _showIcons
    , _showMenubar
    , _showNotFound
    , _showNumbers
    , _showOther
    , _showPreviewEntry
    , _showPrivate
    , _showProgress
    , _showRecommended
    , _showSize
    , _showStyle
    , _showTabs
    , _showText
    , _showTips
    , _showWeekNumbers
    , _singleLineMode
    , _singleNodeValue
    , _singleParagraphMode
    , _size
    , _sizePoints
    , _sizeSet
    , _sizing
    , _skipPagerHint
    , _skipTaskbarHint
    , _snapEdge
    , _snapEdgeSet
    , _snapToLines
    , _snapToTicks
    , _snapshotLength
    , _socketWindow
    , _sortColumnId
    , _sortIndicator
    , _sortOrder
    , _sortType
    , _spacing
    , _span
    , _specified
    , _spellCheckingLanguages
    , _spellcheck
    , _src
    , _srcElement
    , _srcdoc
    , _srcset
    , _stack
    , _standby
    , _start
    , _startContainer
    , _startOffset
    , _startTime
    , _startupId
    , _state
    , _status
    , _statusString
    , _statusbar
    , _statusbarVisible
    , _step
    , _stepIncrement
    , _stepMismatch
    , _stock
    , _stockDetail
    , _stockId
    , _stockSize
    , _storageType
    , _stretch
    , _stretchSet
    , _strikethrough
    , _strikethroughRgba
    , _strikethroughRgbaSet
    , _strikethroughSet
    , _stringValue
    , _style
    , _styleContext
    , _styleMedia
    , _styleSet
    , _styleSheets
    , _submenu
    , _subtitle
    , _suffixes
    , _suggestedFilename
    , _summary
    , _supportSelection
    , _surface
    , _systemId
    , _tBodies
    , _tFoot
    , _tHead
    , _tabIndex
    , _tabKeyCyclesThroughElements
    , _tabPos
    , _tabs
    , _tabsSet
    , _tagName
    , _tagTable
    , _takeFocus
    , _target
    , _targetFrame
    , _tearoffState
    , _tearoffTitle
    , _text
    , _textColumn
    , _textContent
    , _textLength
    , _textLock
    , _textTracks
    , _textUnlock
    , _textXalign
    , _textYalign
    , _timeStamp
    , _timelineProfilingEnabled
    , _timestamp
    , _timing
    , _title
    , _toElement
    , _tooLong
    , _toolbar
    , _toolbarStyle
    , _toolbarVisible
    , _tooltip
    , _tooltipColumn
    , _tooltipLock
    , _tooltipMarkup
    , _tooltipNotAuthorized
    , _tooltipText
    , _tooltipUnlock
    , _top
    , _topPadding
    , _totalFrameDelay
    , _totalJsHeapSize
    , _totalSize
    , _totalVideoFrames
    , _touchOnly
    , _track
    , _trackPrintStatus
    , _trackVisitedLinks
    , _transientFor
    , _transitionDuration
    , _transitionRunning
    , _transitionType
    , _transitionsEnabled
    , _translate
    , _translationDomain
    , _translatorCredits
    , _transparent
    , _trueSpeed
    , _truncateMultiline
    , _type
    , _typeHint
    , _typeMismatch
    , _ui
    , _underline
    , _underlineRgba
    , _underlineRgbaSet
    , _underlineSet
    , _unit
    , _unloadEventEnd
    , _unloadEventStart
    , _updatePolicy
    , _upper
    , _upperStepperSensitivity
    , _urgencyHint
    , _uri
    , _url
    , _useActionAppearance
    , _useAlpha
    , _useFallback
    , _useFont
    , _useFullPage
    , _useHeaderBar
    , _useMap
    , _useMarkup
    , _usePopover
    , _usePreviewLabel
    , _useSize
    , _useStock
    , _useSymbolic
    , _useUnderline
    , _usedJsHeapSize
    , _userAgent
    , _userScalable
    , _userStylesheetUri
    , _vAlign
    , _vLink
    , _vadjustment
    , _valid
    , _validationMessage
    , _validity
    , _valign
    , _value
    , _valueAsNumber
    , _valueMissing
    , _valuePos
    , _valueType
    , _variant
    , _variantSet
    , _vendor
    , _vendorSub
    , _version
    , _vertical
    , _verticalScrollbarPolicy
    , _vexpand
    , _vexpandSet
    , _vhomogeneous
    , _videoHeight
    , _videoTracks
    , _videoWidth
    , _view
    , _viewMode
    , _viewportAttributes
    , _virtualRoot
    , _visibility
    , _visibilityState
    , _visible
    , _visibleChild
    , _visibleChildName
    , _visibleHorizontal
    , _visibleOverflown
    , _visibleSubmenu
    , _visibleVertical
    , _visibleWindow
    , _visited
    , _vlinkColor
    , _volume
    , _vscrollPolicy
    , _vscrollbarPolicy
    , _vspace
    , _webDatabaseQuota
    , _webDatabaseUsage
    , _webInspector
    , _webView
    , _webkitAudioDecodedByteCount
    , _webkitBattery
    , _webkitClosedCaptionsVisible
    , _webkitCurrentFullScreenElement
    , _webkitCurrentPlaybackTargetIsWireless
    , _webkitDecodedFrameCount
    , _webkitDirectionInvertedFromDevice
    , _webkitDisplayingFullscreen
    , _webkitDroppedFrameCount
    , _webkitForce
    , _webkitFullScreenKeyboardInputAllowed
    , _webkitFullscreenElement
    , _webkitFullscreenEnabled
    , _webkitGrammar
    , _webkitHasClosedCaptions
    , _webkitIsFullScreen
    , _webkitMovementX
    , _webkitMovementY
    , _webkitPersistentStorage
    , _webkitPointerLockElement
    , _webkitPreservesPitch
    , _webkitRadiusX
    , _webkitRadiusY
    , _webkitRegionOverset
    , _webkitRelativePath
    , _webkitRotationAngle
    , _webkitSpeech
    , _webkitStorageInfo
    , _webkitSupportsFullscreen
    , _webkitTemporaryStorage
    , _webkitVideoDecodedByteCount
    , _webkitWirelessVideoPlaybackDisabled
    , _webkitdirectory
    , _webkitdropzone
    , _website
    , _websiteLabel
    , _weight
    , _weightSet
    , _whatToShow
    , _wheelDelta
    , _wheelDeltaX
    , _wheelDeltaY
    , _which
    , _wholeText
    , _wideHandle
    , _widget
    , _width
    , _widthChars
    , _widthRequest
    , _willValidate
    , _window
    , _windowFeatures
    , _windowPlacement
    , _windowPlacementSet
    , _windowPosition
    , _wrap
    , _wrapLicense
    , _wrapMode
    , _wrapModeSet
    , _wrapWidth
    , _x
    , _xOffset
    , _xalign
    , _xmlEncoding
    , _xmlStandalone
    , _xmlVersion
    , _xpad
    , _xscale
    , _y
    , _yalign
    , _year
    , _ypad
    , _yscale
    , _zoomLevel
    , _zoomStep
    ) where




import Data.Proxy (Proxy(..))

-- Property "a-link"
_aLink :: Proxy "a-link"
_aLink = Proxy


-- Property "abbr"
_abbr :: Proxy "abbr"
_abbr = Proxy


-- Property "above-child"
_aboveChild :: Proxy "above-child"
_aboveChild = Proxy


-- Property "accel-closure"
_accelClosure :: Proxy "accel-closure"
_accelClosure = Proxy


-- Property "accel-group"
_accelGroup :: Proxy "accel-group"
_accelGroup = Proxy


-- Property "accel-key"
_accelKey :: Proxy "accel-key"
_accelKey = Proxy


-- Property "accel-mode"
_accelMode :: Proxy "accel-mode"
_accelMode = Proxy


-- Property "accel-mods"
_accelMods :: Proxy "accel-mods"
_accelMods = Proxy


-- Property "accel-path"
_accelPath :: Proxy "accel-path"
_accelPath = Proxy


-- Property "accel-widget"
_accelWidget :: Proxy "accel-widget"
_accelWidget = Proxy


-- Property "accept"
_accept :: Proxy "accept"
_accept = Proxy


-- Property "accept-charset"
_acceptCharset :: Proxy "accept-charset"
_acceptCharset = Proxy


-- Property "accept-focus"
_acceptFocus :: Proxy "accept-focus"
_acceptFocus = Proxy


-- Property "accepts-tab"
_acceptsTab :: Proxy "accepts-tab"
_acceptsTab = Proxy


-- Property "access-key"
_accessKey :: Proxy "access-key"
_accessKey = Proxy


-- Property "accumulative-margin"
_accumulativeMargin :: Proxy "accumulative-margin"
_accumulativeMargin = Proxy


-- Property "action"
_action :: Proxy "action"
_action = Proxy


-- Property "action-group"
_actionGroup :: Proxy "action-group"
_actionGroup = Proxy


-- Property "action-name"
_actionName :: Proxy "action-name"
_actionName = Proxy


-- Property "action-target"
_actionTarget :: Proxy "action-target"
_actionTarget = Proxy


-- Property "activatable"
_activatable :: Proxy "activatable"
_activatable = Proxy


-- Property "activate-on-single-click"
_activateOnSingleClick :: Proxy "activate-on-single-click"
_activateOnSingleClick = Proxy


-- Property "activates-default"
_activatesDefault :: Proxy "activates-default"
_activatesDefault = Proxy


-- Property "active"
_active :: Proxy "active"
_active = Proxy


-- Property "active-cues"
_activeCues :: Proxy "active-cues"
_activeCues = Proxy


-- Property "active-element"
_activeElement :: Proxy "active-element"
_activeElement = Proxy


-- Property "active-id"
_activeId :: Proxy "active-id"
_activeId = Proxy


-- Property "active-window"
_activeWindow :: Proxy "active-window"
_activeWindow = Proxy


-- Property "add-tearoffs"
_addTearoffs :: Proxy "add-tearoffs"
_addTearoffs = Proxy


-- Property "adjustment"
_adjustment :: Proxy "adjustment"
_adjustment = Proxy


-- Property "align"
_align :: Proxy "align"
_align = Proxy


-- Property "align-set"
_alignSet :: Proxy "align-set"
_alignSet = Proxy


-- Property "align-widget"
_alignWidget :: Proxy "align-widget"
_alignWidget = Proxy


-- Property "alignment"
_alignment :: Proxy "alignment"
_alignment = Proxy


-- Property "alink-color"
_alinkColor :: Proxy "alink-color"
_alinkColor = Proxy


-- Property "allow-async"
_allowAsync :: Proxy "allow-async"
_allowAsync = Proxy


-- Property "allows-eval"
_allowsEval :: Proxy "allows-eval"
_allowsEval = Proxy


-- Property "allows-inline-script"
_allowsInlineScript :: Proxy "allows-inline-script"
_allowsInlineScript = Proxy


-- Property "allows-inline-style"
_allowsInlineStyle :: Proxy "allows-inline-style"
_allowsInlineStyle = Proxy


-- Property "alpha"
_alpha :: Proxy "alpha"
_alpha = Proxy


-- Property "alt"
_alt :: Proxy "alt"
_alt = Proxy


-- Property "alt-graph-key"
_altGraphKey :: Proxy "alt-graph-key"
_altGraphKey = Proxy


-- Property "alt-key"
_altKey :: Proxy "alt-key"
_altKey = Proxy


-- Property "alternate-title"
_alternateTitle :: Proxy "alternate-title"
_alternateTitle = Proxy


-- Property "always-show-image"
_alwaysShowImage :: Proxy "always-show-image"
_alwaysShowImage = Proxy


-- Property "ancestor-origins"
_ancestorOrigins :: Proxy "ancestor-origins"
_ancestorOrigins = Proxy


-- Property "anchor-node"
_anchorNode :: Proxy "anchor-node"
_anchorNode = Proxy


-- Property "anchor-offset"
_anchorOffset :: Proxy "anchor-offset"
_anchorOffset = Proxy


-- Property "anchors"
_anchors :: Proxy "anchors"
_anchors = Proxy


-- Property "angle"
_angle :: Proxy "angle"
_angle = Proxy


-- Property "app-code-name"
_appCodeName :: Proxy "app-code-name"
_appCodeName = Proxy


-- Property "app-menu"
_appMenu :: Proxy "app-menu"
_appMenu = Proxy


-- Property "app-name"
_appName :: Proxy "app-name"
_appName = Proxy


-- Property "app-paintable"
_appPaintable :: Proxy "app-paintable"
_appPaintable = Proxy


-- Property "app-version"
_appVersion :: Proxy "app-version"
_appVersion = Proxy


-- Property "applets"
_applets :: Proxy "applets"
_applets = Proxy


-- Property "application"
_application :: Proxy "application"
_application = Proxy


-- Property "application-cache"
_applicationCache :: Proxy "application-cache"
_applicationCache = Proxy


-- Property "apply-author-styles"
_applyAuthorStyles :: Proxy "apply-author-styles"
_applyAuthorStyles = Proxy


-- Property "archive"
_archive :: Proxy "archive"
_archive = Proxy


-- Property "area"
_area :: Proxy "area"
_area = Proxy


-- Property "areas"
_areas :: Proxy "areas"
_areas = Proxy


-- Property "arrow-type"
_arrowType :: Proxy "arrow-type"
_arrowType = Proxy


-- Property "artists"
_artists :: Proxy "artists"
_artists = Proxy


-- Property "async"
_async :: Proxy "async"
_async = Proxy


-- Property "attach-widget"
_attachWidget :: Proxy "attach-widget"
_attachWidget = Proxy


-- Property "attached-to"
_attachedTo :: Proxy "attached-to"
_attachedTo = Proxy


-- Property "attributes"
_attributes :: Proxy "attributes"
_attributes = Proxy


-- Property "audio-tracks"
_audioTracks :: Proxy "audio-tracks"
_audioTracks = Proxy


-- Property "authors"
_authors :: Proxy "authors"
_authors = Proxy


-- Property "auto-load-images"
_autoLoadImages :: Proxy "auto-load-images"
_autoLoadImages = Proxy


-- Property "auto-render"
_autoRender :: Proxy "auto-render"
_autoRender = Proxy


-- Property "auto-resize-window"
_autoResizeWindow :: Proxy "auto-resize-window"
_autoResizeWindow = Proxy


-- Property "auto-shrink-images"
_autoShrinkImages :: Proxy "auto-shrink-images"
_autoShrinkImages = Proxy


-- Property "autocapitalize"
_autocapitalize :: Proxy "autocapitalize"
_autocapitalize = Proxy


-- Property "autocomplete"
_autocomplete :: Proxy "autocomplete"
_autocomplete = Proxy


-- Property "autocorrect"
_autocorrect :: Proxy "autocorrect"
_autocorrect = Proxy


-- Property "autofocus"
_autofocus :: Proxy "autofocus"
_autofocus = Proxy


-- Property "autoplay"
_autoplay :: Proxy "autoplay"
_autoplay = Proxy


-- Property "avail-height"
_availHeight :: Proxy "avail-height"
_availHeight = Proxy


-- Property "avail-left"
_availLeft :: Proxy "avail-left"
_availLeft = Proxy


-- Property "avail-top"
_availTop :: Proxy "avail-top"
_availTop = Proxy


-- Property "avail-width"
_availWidth :: Proxy "avail-width"
_availWidth = Proxy


-- Property "available-height"
_availableHeight :: Proxy "available-height"
_availableHeight = Proxy


-- Property "available-width"
_availableWidth :: Proxy "available-width"
_availableWidth = Proxy


-- Property "axis"
_axis :: Proxy "axis"
_axis = Proxy


-- Property "background"
_background :: Proxy "background"
_background = Proxy


-- Property "background-full-height"
_backgroundFullHeight :: Proxy "background-full-height"
_backgroundFullHeight = Proxy


-- Property "background-full-height-set"
_backgroundFullHeightSet :: Proxy "background-full-height-set"
_backgroundFullHeightSet = Proxy


-- Property "background-gdk"
_backgroundGdk :: Proxy "background-gdk"
_backgroundGdk = Proxy


-- Property "background-icon"
_backgroundIcon :: Proxy "background-icon"
_backgroundIcon = Proxy


-- Property "background-icon-name"
_backgroundIconName :: Proxy "background-icon-name"
_backgroundIconName = Proxy


-- Property "background-rgba"
_backgroundRgba :: Proxy "background-rgba"
_backgroundRgba = Proxy


-- Property "background-set"
_backgroundSet :: Proxy "background-set"
_backgroundSet = Proxy


-- Property "bad-input"
_badInput :: Proxy "bad-input"
_badInput = Proxy


-- Property "base-node"
_baseNode :: Proxy "base-node"
_baseNode = Proxy


-- Property "base-offset"
_baseOffset :: Proxy "base-offset"
_baseOffset = Proxy


-- Property "base-uri"
_baseUri :: Proxy "base-uri"
_baseUri = Proxy


-- Property "baseline-position"
_baselinePosition :: Proxy "baseline-position"
_baselinePosition = Proxy


-- Property "baseline-row"
_baselineRow :: Proxy "baseline-row"
_baselineRow = Proxy


-- Property "behavior"
_behavior :: Proxy "behavior"
_behavior = Proxy


-- Property "bg-color"
_bgColor :: Proxy "bg-color"
_bgColor = Proxy


-- Property "body"
_body :: Proxy "body"
_body = Proxy


-- Property "boolean-value"
_booleanValue :: Proxy "boolean-value"
_booleanValue = Proxy


-- Property "border"
_border :: Proxy "border"
_border = Proxy


-- Property "border-width"
_borderWidth :: Proxy "border-width"
_borderWidth = Proxy


-- Property "bottom-padding"
_bottomPadding :: Proxy "bottom-padding"
_bottomPadding = Proxy


-- Property "bubbles"
_bubbles :: Proxy "bubbles"
_bubbles = Proxy


-- Property "buffer"
_buffer :: Proxy "buffer"
_buffer = Proxy


-- Property "buffered"
_buffered :: Proxy "buffered"
_buffered = Proxy


-- Property "button"
_button :: Proxy "button"
_button = Proxy


-- Property "button-sensitivity"
_buttonSensitivity :: Proxy "button-sensitivity"
_buttonSensitivity = Proxy


-- Property "buttons"
_buttons :: Proxy "buttons"
_buttons = Proxy


-- Property "can-default"
_canDefault :: Proxy "can-default"
_canDefault = Proxy


-- Property "can-focus"
_canFocus :: Proxy "can-focus"
_canFocus = Proxy


-- Property "cancel-bubble"
_cancelBubble :: Proxy "cancel-bubble"
_cancelBubble = Proxy


-- Property "cancel-button"
_cancelButton :: Proxy "cancel-button"
_cancelButton = Proxy


-- Property "cancelable"
_cancelable :: Proxy "cancelable"
_cancelable = Proxy


-- Property "caps-lock-warning"
_capsLockWarning :: Proxy "caps-lock-warning"
_capsLockWarning = Proxy


-- Property "caption"
_caption :: Proxy "caption"
_caption = Proxy


-- Property "capture"
_capture :: Proxy "capture"
_capture = Proxy


-- Property "cell-area"
_cellArea :: Proxy "cell-area"
_cellArea = Proxy


-- Property "cell-area-context"
_cellAreaContext :: Proxy "cell-area-context"
_cellAreaContext = Proxy


-- Property "cell-background"
_cellBackground :: Proxy "cell-background"
_cellBackground = Proxy


-- Property "cell-background-gdk"
_cellBackgroundGdk :: Proxy "cell-background-gdk"
_cellBackgroundGdk = Proxy


-- Property "cell-background-rgba"
_cellBackgroundRgba :: Proxy "cell-background-rgba"
_cellBackgroundRgba = Proxy


-- Property "cell-background-set"
_cellBackgroundSet :: Proxy "cell-background-set"
_cellBackgroundSet = Proxy


-- Property "cell-index"
_cellIndex :: Proxy "cell-index"
_cellIndex = Proxy


-- Property "cell-padding"
_cellPadding :: Proxy "cell-padding"
_cellPadding = Proxy


-- Property "cell-spacing"
_cellSpacing :: Proxy "cell-spacing"
_cellSpacing = Proxy


-- Property "cells"
_cells :: Proxy "cells"
_cells = Proxy


-- Property "centered"
_centered :: Proxy "centered"
_centered = Proxy


-- Property "ch"
_ch :: Proxy "ch"
_ch = Proxy


-- Property "ch-off"
_chOff :: Proxy "ch-off"
_chOff = Proxy


-- Property "challenge"
_challenge :: Proxy "challenge"
_challenge = Proxy


-- Property "char-code"
_charCode :: Proxy "char-code"
_charCode = Proxy


-- Property "character-set"
_characterSet :: Proxy "character-set"
_characterSet = Proxy


-- Property "charging"
_charging :: Proxy "charging"
_charging = Proxy


-- Property "charging-time"
_chargingTime :: Proxy "charging-time"
_chargingTime = Proxy


-- Property "charset"
_charset :: Proxy "charset"
_charset = Proxy


-- Property "checked"
_checked :: Proxy "checked"
_checked = Proxy


-- Property "child"
_child :: Proxy "child"
_child = Proxy


-- Property "child-detached"
_childDetached :: Proxy "child-detached"
_childDetached = Proxy


-- Property "child-element-count"
_childElementCount :: Proxy "child-element-count"
_childElementCount = Proxy


-- Property "child-model"
_childModel :: Proxy "child-model"
_childModel = Proxy


-- Property "child-nodes"
_childNodes :: Proxy "child-nodes"
_childNodes = Proxy


-- Property "child-pack-direction"
_childPackDirection :: Proxy "child-pack-direction"
_childPackDirection = Proxy


-- Property "child-revealed"
_childRevealed :: Proxy "child-revealed"
_childRevealed = Proxy


-- Property "children"
_children :: Proxy "children"
_children = Proxy


-- Property "cite"
_cite :: Proxy "cite"
_cite = Proxy


-- Property "class-list"
_classList :: Proxy "class-list"
_classList = Proxy


-- Property "class-name"
_className :: Proxy "class-name"
_className = Proxy


-- Property "clear"
_clear :: Proxy "clear"
_clear = Proxy


-- Property "clickable"
_clickable :: Proxy "clickable"
_clickable = Proxy


-- Property "client-height"
_clientHeight :: Proxy "client-height"
_clientHeight = Proxy


-- Property "client-information"
_clientInformation :: Proxy "client-information"
_clientInformation = Proxy


-- Property "client-left"
_clientLeft :: Proxy "client-left"
_clientLeft = Proxy


-- Property "client-top"
_clientTop :: Proxy "client-top"
_clientTop = Proxy


-- Property "client-width"
_clientWidth :: Proxy "client-width"
_clientWidth = Proxy


-- Property "client-x"
_clientX :: Proxy "client-x"
_clientX = Proxy


-- Property "client-y"
_clientY :: Proxy "client-y"
_clientY = Proxy


-- Property "climb-rate"
_climbRate :: Proxy "climb-rate"
_climbRate = Proxy


-- Property "closed"
_closed :: Proxy "closed"
_closed = Proxy


-- Property "code"
_code :: Proxy "code"
_code = Proxy


-- Property "code-base"
_codeBase :: Proxy "code-base"
_codeBase = Proxy


-- Property "code-type"
_codeType :: Proxy "code-type"
_codeType = Proxy


-- Property "col-span"
_colSpan :: Proxy "col-span"
_colSpan = Proxy


-- Property "collapsed"
_collapsed :: Proxy "collapsed"
_collapsed = Proxy


-- Property "color"
_color :: Proxy "color"
_color = Proxy


-- Property "color-depth"
_colorDepth :: Proxy "color-depth"
_colorDepth = Proxy


-- Property "color-hash"
_colorHash :: Proxy "color-hash"
_colorHash = Proxy


-- Property "color-selection"
_colorSelection :: Proxy "color-selection"
_colorSelection = Proxy


-- Property "cols"
_cols :: Proxy "cols"
_cols = Proxy


-- Property "column-homogeneous"
_columnHomogeneous :: Proxy "column-homogeneous"
_columnHomogeneous = Proxy


-- Property "column-spacing"
_columnSpacing :: Proxy "column-spacing"
_columnSpacing = Proxy


-- Property "column-span-column"
_columnSpanColumn :: Proxy "column-span-column"
_columnSpanColumn = Proxy


-- Property "columns"
_columns :: Proxy "columns"
_columns = Proxy


-- Property "comments"
_comments :: Proxy "comments"
_comments = Proxy


-- Property "common-ancestor-container"
_commonAncestorContainer :: Proxy "common-ancestor-container"
_commonAncestorContainer = Proxy


-- Property "compact"
_compact :: Proxy "compact"
_compact = Proxy


-- Property "compat-mode"
_compatMode :: Proxy "compat-mode"
_compatMode = Proxy


-- Property "complete"
_complete :: Proxy "complete"
_complete = Proxy


-- Property "completion"
_completion :: Proxy "completion"
_completion = Proxy


-- Property "composite-child"
_compositeChild :: Proxy "composite-child"
_compositeChild = Proxy


-- Property "connect-end"
_connectEnd :: Proxy "connect-end"
_connectEnd = Proxy


-- Property "connect-start"
_connectStart :: Proxy "connect-start"
_connectStart = Proxy


-- Property "console"
_console :: Proxy "console"
_console = Proxy


-- Property "content"
_content :: Proxy "content"
_content = Proxy


-- Property "content-document"
_contentDocument :: Proxy "content-document"
_contentDocument = Proxy


-- Property "content-editable"
_contentEditable :: Proxy "content-editable"
_contentEditable = Proxy


-- Property "content-type"
_contentType :: Proxy "content-type"
_contentType = Proxy


-- Property "content-window"
_contentWindow :: Proxy "content-window"
_contentWindow = Proxy


-- Property "context"
_context :: Proxy "context"
_context = Proxy


-- Property "control"
_control :: Proxy "control"
_control = Proxy


-- Property "controller"
_controller :: Proxy "controller"
_controller = Proxy


-- Property "controls"
_controls :: Proxy "controls"
_controls = Proxy


-- Property "cookie"
_cookie :: Proxy "cookie"
_cookie = Proxy


-- Property "cookie-enabled"
_cookieEnabled :: Proxy "cookie-enabled"
_cookieEnabled = Proxy


-- Property "coords"
_coords :: Proxy "coords"
_coords = Proxy


-- Property "copy-target-list"
_copyTargetList :: Proxy "copy-target-list"
_copyTargetList = Proxy


-- Property "copyright"
_copyright :: Proxy "copyright"
_copyright = Proxy


-- Property "core-object"
_coreObject :: Proxy "core-object"
_coreObject = Proxy


-- Property "corrupted-video-frames"
_corruptedVideoFrames :: Proxy "corrupted-video-frames"
_corruptedVideoFrames = Proxy


-- Property "count"
_count :: Proxy "count"
_count = Proxy


-- Property "create-folders"
_createFolders :: Proxy "create-folders"
_createFolders = Proxy


-- Property "creation-time"
_creationTime :: Proxy "creation-time"
_creationTime = Proxy


-- Property "cross-origin"
_crossOrigin :: Proxy "cross-origin"
_crossOrigin = Proxy


-- Property "css"
_css :: Proxy "css"
_css = Proxy


-- Property "css-rules"
_cssRules :: Proxy "css-rules"
_cssRules = Proxy


-- Property "css-text"
_cssText :: Proxy "css-text"
_cssText = Proxy


-- Property "css-value-type"
_cssValueType :: Proxy "css-value-type"
_cssValueType = Proxy


-- Property "ctrl-key"
_ctrlKey :: Proxy "ctrl-key"
_ctrlKey = Proxy


-- Property "cues"
_cues :: Proxy "cues"
_cues = Proxy


-- Property "current-alpha"
_currentAlpha :: Proxy "current-alpha"
_currentAlpha = Proxy


-- Property "current-color"
_currentColor :: Proxy "current-color"
_currentColor = Proxy


-- Property "current-node"
_currentNode :: Proxy "current-node"
_currentNode = Proxy


-- Property "current-page"
_currentPage :: Proxy "current-page"
_currentPage = Proxy


-- Property "current-rgba"
_currentRgba :: Proxy "current-rgba"
_currentRgba = Proxy


-- Property "current-script"
_currentScript :: Proxy "current-script"
_currentScript = Proxy


-- Property "current-size"
_currentSize :: Proxy "current-size"
_currentSize = Proxy


-- Property "current-src"
_currentSrc :: Proxy "current-src"
_currentSrc = Proxy


-- Property "current-target"
_currentTarget :: Proxy "current-target"
_currentTarget = Proxy


-- Property "current-time"
_currentTime :: Proxy "current-time"
_currentTime = Proxy


-- Property "current-value"
_currentValue :: Proxy "current-value"
_currentValue = Proxy


-- Property "cursive-font-family"
_cursiveFontFamily :: Proxy "cursive-font-family"
_cursiveFontFamily = Proxy


-- Property "cursor-position"
_cursorPosition :: Proxy "cursor-position"
_cursorPosition = Proxy


-- Property "cursor-visible"
_cursorVisible :: Proxy "cursor-visible"
_cursorVisible = Proxy


-- Property "custom-encoding"
_customEncoding :: Proxy "custom-encoding"
_customEncoding = Proxy


-- Property "custom-error"
_customError :: Proxy "custom-error"
_customError = Proxy


-- Property "custom-tab-label"
_customTabLabel :: Proxy "custom-tab-label"
_customTabLabel = Proxy


-- Property "custom-title"
_customTitle :: Proxy "custom-title"
_customTitle = Proxy


-- Property "data"
_data :: Proxy "data"
_data = Proxy


-- Property "date-time"
_dateTime :: Proxy "date-time"
_dateTime = Proxy


-- Property "day"
_day :: Proxy "day"
_day = Proxy


-- Property "declare"
_declare :: Proxy "declare"
_declare = Proxy


-- Property "decorated"
_decorated :: Proxy "decorated"
_decorated = Proxy


-- Property "decoration-layout"
_decorationLayout :: Proxy "decoration-layout"
_decorationLayout = Proxy


-- Property "decoration-layout-set"
_decorationLayoutSet :: Proxy "decoration-layout-set"
_decorationLayoutSet = Proxy


-- Property "default-charset"
_defaultCharset :: Proxy "default-charset"
_defaultCharset = Proxy


-- Property "default-checked"
_defaultChecked :: Proxy "default-checked"
_defaultChecked = Proxy


-- Property "default-encoding"
_defaultEncoding :: Proxy "default-encoding"
_defaultEncoding = Proxy


-- Property "default-font-family"
_defaultFontFamily :: Proxy "default-font-family"
_defaultFontFamily = Proxy


-- Property "default-font-size"
_defaultFontSize :: Proxy "default-font-size"
_defaultFontSize = Proxy


-- Property "default-height"
_defaultHeight :: Proxy "default-height"
_defaultHeight = Proxy


-- Property "default-monospace-font-size"
_defaultMonospaceFontSize :: Proxy "default-monospace-font-size"
_defaultMonospaceFontSize = Proxy


-- Property "default-muted"
_defaultMuted :: Proxy "default-muted"
_defaultMuted = Proxy


-- Property "default-page-setup"
_defaultPageSetup :: Proxy "default-page-setup"
_defaultPageSetup = Proxy


-- Property "default-playback-rate"
_defaultPlaybackRate :: Proxy "default-playback-rate"
_defaultPlaybackRate = Proxy


-- Property "default-prevented"
_defaultPrevented :: Proxy "default-prevented"
_defaultPrevented = Proxy


-- Property "default-selected"
_defaultSelected :: Proxy "default-selected"
_defaultSelected = Proxy


-- Property "default-status"
_defaultStatus :: Proxy "default-status"
_defaultStatus = Proxy


-- Property "default-text"
_defaultText :: Proxy "default-text"
_defaultText = Proxy


-- Property "default-value"
_defaultValue :: Proxy "default-value"
_defaultValue = Proxy


-- Property "default-view"
_defaultView :: Proxy "default-view"
_defaultView = Proxy


-- Property "default-width"
_defaultWidth :: Proxy "default-width"
_defaultWidth = Proxy


-- Property "defer"
_defer :: Proxy "defer"
_defer = Proxy


-- Property "delay-factor"
_delayFactor :: Proxy "delay-factor"
_delayFactor = Proxy


-- Property "deletable"
_deletable :: Proxy "deletable"
_deletable = Proxy


-- Property "delta-mode"
_deltaMode :: Proxy "delta-mode"
_deltaMode = Proxy


-- Property "delta-x"
_deltaX :: Proxy "delta-x"
_deltaX = Proxy


-- Property "delta-y"
_deltaY :: Proxy "delta-y"
_deltaY = Proxy


-- Property "delta-z"
_deltaZ :: Proxy "delta-z"
_deltaZ = Proxy


-- Property "description"
_description :: Proxy "description"
_description = Proxy


-- Property "design-mode"
_designMode :: Proxy "design-mode"
_designMode = Proxy


-- Property "desktop-width"
_desktopWidth :: Proxy "desktop-width"
_desktopWidth = Proxy


-- Property "destination-uri"
_destinationUri :: Proxy "destination-uri"
_destinationUri = Proxy


-- Property "destroy-with-parent"
_destroyWithParent :: Proxy "destroy-with-parent"
_destroyWithParent = Proxy


-- Property "detail"
_detail :: Proxy "detail"
_detail = Proxy


-- Property "detail-height-rows"
_detailHeightRows :: Proxy "detail-height-rows"
_detailHeightRows = Proxy


-- Property "detail-width-chars"
_detailWidthChars :: Proxy "detail-width-chars"
_detailWidthChars = Proxy


-- Property "device-dpi"
_deviceDpi :: Proxy "device-dpi"
_deviceDpi = Proxy


-- Property "device-height"
_deviceHeight :: Proxy "device-height"
_deviceHeight = Proxy


-- Property "device-pixel-ratio"
_devicePixelRatio :: Proxy "device-pixel-ratio"
_devicePixelRatio = Proxy


-- Property "device-width"
_deviceWidth :: Proxy "device-width"
_deviceWidth = Proxy


-- Property "dialog"
_dialog :: Proxy "dialog"
_dialog = Proxy


-- Property "digits"
_digits :: Proxy "digits"
_digits = Proxy


-- Property "dir"
_dir :: Proxy "dir"
_dir = Proxy


-- Property "dir-name"
_dirName :: Proxy "dir-name"
_dirName = Proxy


-- Property "direction"
_direction :: Proxy "direction"
_direction = Proxy


-- Property "disabled"
_disabled :: Proxy "disabled"
_disabled = Proxy


-- Property "discharging-time"
_dischargingTime :: Proxy "discharging-time"
_dischargingTime = Proxy


-- Property "display-name"
_displayName :: Proxy "display-name"
_displayName = Proxy


-- Property "do-overwrite-confirmation"
_doOverwriteConfirmation :: Proxy "do-overwrite-confirmation"
_doOverwriteConfirmation = Proxy


-- Property "doctype"
_doctype :: Proxy "doctype"
_doctype = Proxy


-- Property "document"
_document :: Proxy "document"
_document = Proxy


-- Property "document-element"
_documentElement :: Proxy "document-element"
_documentElement = Proxy


-- Property "document-uri"
_documentUri :: Proxy "document-uri"
_documentUri = Proxy


-- Property "documenters"
_documenters :: Proxy "documenters"
_documenters = Proxy


-- Property "dom-complete"
_domComplete :: Proxy "dom-complete"
_domComplete = Proxy


-- Property "dom-content-loaded-event-end"
_domContentLoadedEventEnd :: Proxy "dom-content-loaded-event-end"
_domContentLoadedEventEnd = Proxy


-- Property "dom-content-loaded-event-start"
_domContentLoadedEventStart :: Proxy "dom-content-loaded-event-start"
_domContentLoadedEventStart = Proxy


-- Property "dom-interactive"
_domInteractive :: Proxy "dom-interactive"
_domInteractive = Proxy


-- Property "dom-loading"
_domLoading :: Proxy "dom-loading"
_domLoading = Proxy


-- Property "domain"
_domain :: Proxy "domain"
_domain = Proxy


-- Property "domain-lookup-end"
_domainLookupEnd :: Proxy "domain-lookup-end"
_domainLookupEnd = Proxy


-- Property "domain-lookup-start"
_domainLookupStart :: Proxy "domain-lookup-start"
_domainLookupStart = Proxy


-- Property "double-buffered"
_doubleBuffered :: Proxy "double-buffered"
_doubleBuffered = Proxy


-- Property "download"
_download :: Proxy "download"
_download = Proxy


-- Property "draggable"
_draggable :: Proxy "draggable"
_draggable = Proxy


-- Property "draw"
_draw :: Proxy "draw"
_draw = Proxy


-- Property "draw-as-radio"
_drawAsRadio :: Proxy "draw-as-radio"
_drawAsRadio = Proxy


-- Property "draw-indicator"
_drawIndicator :: Proxy "draw-indicator"
_drawIndicator = Proxy


-- Property "draw-sensitive"
_drawSensitive :: Proxy "draw-sensitive"
_drawSensitive = Proxy


-- Property "draw-value"
_drawValue :: Proxy "draw-value"
_drawValue = Proxy


-- Property "dropped-video-frames"
_droppedVideoFrames :: Proxy "dropped-video-frames"
_droppedVideoFrames = Proxy


-- Property "duration"
_duration :: Proxy "duration"
_duration = Proxy


-- Property "edit-widget"
_editWidget :: Proxy "edit-widget"
_editWidget = Proxy


-- Property "editable"
_editable :: Proxy "editable"
_editable = Proxy


-- Property "editable-set"
_editableSet :: Proxy "editable-set"
_editableSet = Proxy


-- Property "edited-cell"
_editedCell :: Proxy "edited-cell"
_editedCell = Proxy


-- Property "editing"
_editing :: Proxy "editing"
_editing = Proxy


-- Property "editing-behavior"
_editingBehavior :: Proxy "editing-behavior"
_editingBehavior = Proxy


-- Property "editing-canceled"
_editingCanceled :: Proxy "editing-canceled"
_editingCanceled = Proxy


-- Property "elements"
_elements :: Proxy "elements"
_elements = Proxy


-- Property "ellipsize"
_ellipsize :: Proxy "ellipsize"
_ellipsize = Proxy


-- Property "ellipsize-set"
_ellipsizeSet :: Proxy "ellipsize-set"
_ellipsizeSet = Proxy


-- Property "embed-page-setup"
_embedPageSetup :: Proxy "embed-page-setup"
_embedPageSetup = Proxy


-- Property "embedded"
_embedded :: Proxy "embedded"
_embedded = Proxy


-- Property "embeds"
_embeds :: Proxy "embeds"
_embeds = Proxy


-- Property "enable-accelerated-compositing"
_enableAcceleratedCompositing :: Proxy "enable-accelerated-compositing"
_enableAcceleratedCompositing = Proxy


-- Property "enable-caret-browsing"
_enableCaretBrowsing :: Proxy "enable-caret-browsing"
_enableCaretBrowsing = Proxy


-- Property "enable-default-context-menu"
_enableDefaultContextMenu :: Proxy "enable-default-context-menu"
_enableDefaultContextMenu = Proxy


-- Property "enable-developer-extras"
_enableDeveloperExtras :: Proxy "enable-developer-extras"
_enableDeveloperExtras = Proxy


-- Property "enable-display-of-insecure-content"
_enableDisplayOfInsecureContent :: Proxy "enable-display-of-insecure-content"
_enableDisplayOfInsecureContent = Proxy


-- Property "enable-dns-prefetching"
_enableDnsPrefetching :: Proxy "enable-dns-prefetching"
_enableDnsPrefetching = Proxy


-- Property "enable-dom-paste"
_enableDomPaste :: Proxy "enable-dom-paste"
_enableDomPaste = Proxy


-- Property "enable-file-access-from-file-uris"
_enableFileAccessFromFileUris :: Proxy "enable-file-access-from-file-uris"
_enableFileAccessFromFileUris = Proxy


-- Property "enable-frame-flattening"
_enableFrameFlattening :: Proxy "enable-frame-flattening"
_enableFrameFlattening = Proxy


-- Property "enable-fullscreen"
_enableFullscreen :: Proxy "enable-fullscreen"
_enableFullscreen = Proxy


-- Property "enable-grid-lines"
_enableGridLines :: Proxy "enable-grid-lines"
_enableGridLines = Proxy


-- Property "enable-html5-database"
_enableHtml5Database :: Proxy "enable-html5-database"
_enableHtml5Database = Proxy


-- Property "enable-html5-local-storage"
_enableHtml5LocalStorage :: Proxy "enable-html5-local-storage"
_enableHtml5LocalStorage = Proxy


-- Property "enable-hyperlink-auditing"
_enableHyperlinkAuditing :: Proxy "enable-hyperlink-auditing"
_enableHyperlinkAuditing = Proxy


-- Property "enable-java-applet"
_enableJavaApplet :: Proxy "enable-java-applet"
_enableJavaApplet = Proxy


-- Property "enable-media-stream"
_enableMediaStream :: Proxy "enable-media-stream"
_enableMediaStream = Proxy


-- Property "enable-mediasource"
_enableMediasource :: Proxy "enable-mediasource"
_enableMediasource = Proxy


-- Property "enable-offline-web-application-cache"
_enableOfflineWebApplicationCache :: Proxy "enable-offline-web-application-cache"
_enableOfflineWebApplicationCache = Proxy


-- Property "enable-page-cache"
_enablePageCache :: Proxy "enable-page-cache"
_enablePageCache = Proxy


-- Property "enable-plugins"
_enablePlugins :: Proxy "enable-plugins"
_enablePlugins = Proxy


-- Property "enable-popup"
_enablePopup :: Proxy "enable-popup"
_enablePopup = Proxy


-- Property "enable-private-browsing"
_enablePrivateBrowsing :: Proxy "enable-private-browsing"
_enablePrivateBrowsing = Proxy


-- Property "enable-running-of-insecure-content"
_enableRunningOfInsecureContent :: Proxy "enable-running-of-insecure-content"
_enableRunningOfInsecureContent = Proxy


-- Property "enable-scripts"
_enableScripts :: Proxy "enable-scripts"
_enableScripts = Proxy


-- Property "enable-search"
_enableSearch :: Proxy "enable-search"
_enableSearch = Proxy


-- Property "enable-site-specific-quirks"
_enableSiteSpecificQuirks :: Proxy "enable-site-specific-quirks"
_enableSiteSpecificQuirks = Proxy


-- Property "enable-smooth-scrolling"
_enableSmoothScrolling :: Proxy "enable-smooth-scrolling"
_enableSmoothScrolling = Proxy


-- Property "enable-spatial-navigation"
_enableSpatialNavigation :: Proxy "enable-spatial-navigation"
_enableSpatialNavigation = Proxy


-- Property "enable-spell-checking"
_enableSpellChecking :: Proxy "enable-spell-checking"
_enableSpellChecking = Proxy


-- Property "enable-tree-lines"
_enableTreeLines :: Proxy "enable-tree-lines"
_enableTreeLines = Proxy


-- Property "enable-universal-access-from-file-uris"
_enableUniversalAccessFromFileUris :: Proxy "enable-universal-access-from-file-uris"
_enableUniversalAccessFromFileUris = Proxy


-- Property "enable-webaudio"
_enableWebaudio :: Proxy "enable-webaudio"
_enableWebaudio = Proxy


-- Property "enable-webgl"
_enableWebgl :: Proxy "enable-webgl"
_enableWebgl = Proxy


-- Property "enable-xss-auditor"
_enableXssAuditor :: Proxy "enable-xss-auditor"
_enableXssAuditor = Proxy


-- Property "enabled"
_enabled :: Proxy "enabled"
_enabled = Proxy


-- Property "enabled-plugin"
_enabledPlugin :: Proxy "enabled-plugin"
_enabledPlugin = Proxy


-- Property "encoding"
_encoding :: Proxy "encoding"
_encoding = Proxy


-- Property "enctype"
_enctype :: Proxy "enctype"
_enctype = Proxy


-- Property "end-container"
_endContainer :: Proxy "end-container"
_endContainer = Proxy


-- Property "end-offset"
_endOffset :: Proxy "end-offset"
_endOffset = Proxy


-- Property "end-time"
_endTime :: Proxy "end-time"
_endTime = Proxy


-- Property "ended"
_ended :: Proxy "ended"
_ended = Proxy


-- Property "enforce-96-dpi"
_enforce96Dpi :: Proxy "enforce-96-dpi"
_enforce96Dpi = Proxy


-- Property "entities"
_entities :: Proxy "entities"
_entities = Proxy


-- Property "entry-text-column"
_entryTextColumn :: Proxy "entry-text-column"
_entryTextColumn = Proxy


-- Property "entry-type"
_entryType :: Proxy "entry-type"
_entryType = Proxy


-- Property "error"
_error :: Proxy "error"
_error = Proxy


-- Property "event"
_event :: Proxy "event"
_event = Proxy


-- Property "event-phase"
_eventPhase :: Proxy "event-phase"
_eventPhase = Proxy


-- Property "events"
_events :: Proxy "events"
_events = Proxy


-- Property "exclusive"
_exclusive :: Proxy "exclusive"
_exclusive = Proxy


-- Property "expand"
_expand :: Proxy "expand"
_expand = Proxy


-- Property "expand-entity-references"
_expandEntityReferences :: Proxy "expand-entity-references"
_expandEntityReferences = Proxy


-- Property "expanded"
_expanded :: Proxy "expanded"
_expanded = Proxy


-- Property "expander-column"
_expanderColumn :: Proxy "expander-column"
_expanderColumn = Proxy


-- Property "expected-size"
_expectedSize :: Proxy "expected-size"
_expectedSize = Proxy


-- Property "export-filename"
_exportFilename :: Proxy "export-filename"
_exportFilename = Proxy


-- Property "extent-node"
_extentNode :: Proxy "extent-node"
_extentNode = Proxy


-- Property "extent-offset"
_extentOffset :: Proxy "extent-offset"
_extentOffset = Proxy


-- Property "extra-widget"
_extraWidget :: Proxy "extra-widget"
_extraWidget = Proxy


-- Property "face"
_face :: Proxy "face"
_face = Proxy


-- Property "fallback"
_fallback :: Proxy "fallback"
_fallback = Proxy


-- Property "fallback-set"
_fallbackSet :: Proxy "fallback-set"
_fallbackSet = Proxy


-- Property "family"
_family :: Proxy "family"
_family = Proxy


-- Property "family-set"
_familySet :: Proxy "family-set"
_familySet = Proxy


-- Property "fantasy-font-family"
_fantasyFontFamily :: Proxy "fantasy-font-family"
_fantasyFontFamily = Proxy


-- Property "fetch-start"
_fetchStart :: Proxy "fetch-start"
_fetchStart = Proxy


-- Property "fg-color"
_fgColor :: Proxy "fg-color"
_fgColor = Proxy


-- Property "file"
_file :: Proxy "file"
_file = Proxy


-- Property "filename"
_filename :: Proxy "filename"
_filename = Proxy


-- Property "files"
_files :: Proxy "files"
_files = Proxy


-- Property "fill-level"
_fillLevel :: Proxy "fill-level"
_fillLevel = Proxy


-- Property "filter"
_filter :: Proxy "filter"
_filter = Proxy


-- Property "first-child"
_firstChild :: Proxy "first-child"
_firstChild = Proxy


-- Property "first-element-child"
_firstElementChild :: Proxy "first-element-child"
_firstElementChild = Proxy


-- Property "first-empty-region-index"
_firstEmptyRegionIndex :: Proxy "first-empty-region-index"
_firstEmptyRegionIndex = Proxy


-- Property "fit-model"
_fitModel :: Proxy "fit-model"
_fitModel = Proxy


-- Property "fixed-height-mode"
_fixedHeightMode :: Proxy "fixed-height-mode"
_fixedHeightMode = Proxy


-- Property "fixed-width"
_fixedWidth :: Proxy "fixed-width"
_fixedWidth = Proxy


-- Property "focus-cell"
_focusCell :: Proxy "focus-cell"
_focusCell = Proxy


-- Property "focus-node"
_focusNode :: Proxy "focus-node"
_focusNode = Proxy


-- Property "focus-offset"
_focusOffset :: Proxy "focus-offset"
_focusOffset = Proxy


-- Property "focus-on-click"
_focusOnClick :: Proxy "focus-on-click"
_focusOnClick = Proxy


-- Property "focus-on-map"
_focusOnMap :: Proxy "focus-on-map"
_focusOnMap = Proxy


-- Property "focus-visible"
_focusVisible :: Proxy "focus-visible"
_focusVisible = Proxy


-- Property "follow-state"
_followState :: Proxy "follow-state"
_followState = Proxy


-- Property "font"
_font :: Proxy "font"
_font = Proxy


-- Property "font-desc"
_fontDesc :: Proxy "font-desc"
_fontDesc = Proxy


-- Property "font-name"
_fontName :: Proxy "font-name"
_fontName = Proxy


-- Property "foreground"
_foreground :: Proxy "foreground"
_foreground = Proxy


-- Property "foreground-gdk"
_foregroundGdk :: Proxy "foreground-gdk"
_foregroundGdk = Proxy


-- Property "foreground-rgba"
_foregroundRgba :: Proxy "foreground-rgba"
_foregroundRgba = Proxy


-- Property "foreground-set"
_foregroundSet :: Proxy "foreground-set"
_foregroundSet = Proxy


-- Property "form"
_form :: Proxy "form"
_form = Proxy


-- Property "form-action"
_formAction :: Proxy "form-action"
_formAction = Proxy


-- Property "form-enctype"
_formEnctype :: Proxy "form-enctype"
_formEnctype = Proxy


-- Property "form-method"
_formMethod :: Proxy "form-method"
_formMethod = Proxy


-- Property "form-no-validate"
_formNoValidate :: Proxy "form-no-validate"
_formNoValidate = Proxy


-- Property "form-target"
_formTarget :: Proxy "form-target"
_formTarget = Proxy


-- Property "forms"
_forms :: Proxy "forms"
_forms = Proxy


-- Property "fraction"
_fraction :: Proxy "fraction"
_fraction = Proxy


-- Property "frame"
_frame :: Proxy "frame"
_frame = Proxy


-- Property "frame-border"
_frameBorder :: Proxy "frame-border"
_frameBorder = Proxy


-- Property "frame-element"
_frameElement :: Proxy "frame-element"
_frameElement = Proxy


-- Property "frame-name"
_frameName :: Proxy "frame-name"
_frameName = Proxy


-- Property "frames"
_frames :: Proxy "frames"
_frames = Proxy


-- Property "from-element"
_fromElement :: Proxy "from-element"
_fromElement = Proxy


-- Property "full-content-zoom"
_fullContentZoom :: Proxy "full-content-zoom"
_fullContentZoom = Proxy


-- Property "fullscreen"
_fullscreen :: Proxy "fullscreen"
_fullscreen = Proxy


-- Property "geolocation"
_geolocation :: Proxy "geolocation"
_geolocation = Proxy


-- Property "gfile"
_gfile :: Proxy "gfile"
_gfile = Proxy


-- Property "gicon"
_gicon :: Proxy "gicon"
_gicon = Proxy


-- Property "gravity"
_gravity :: Proxy "gravity"
_gravity = Proxy


-- Property "group"
_group :: Proxy "group"
_group = Proxy


-- Property "group-name"
_groupName :: Proxy "group-name"
_groupName = Proxy


-- Property "gtk-alternative-button-order"
_gtkAlternativeButtonOrder :: Proxy "gtk-alternative-button-order"
_gtkAlternativeButtonOrder = Proxy


-- Property "gtk-alternative-sort-arrows"
_gtkAlternativeSortArrows :: Proxy "gtk-alternative-sort-arrows"
_gtkAlternativeSortArrows = Proxy


-- Property "gtk-application-prefer-dark-theme"
_gtkApplicationPreferDarkTheme :: Proxy "gtk-application-prefer-dark-theme"
_gtkApplicationPreferDarkTheme = Proxy


-- Property "gtk-auto-mnemonics"
_gtkAutoMnemonics :: Proxy "gtk-auto-mnemonics"
_gtkAutoMnemonics = Proxy


-- Property "gtk-button-images"
_gtkButtonImages :: Proxy "gtk-button-images"
_gtkButtonImages = Proxy


-- Property "gtk-can-change-accels"
_gtkCanChangeAccels :: Proxy "gtk-can-change-accels"
_gtkCanChangeAccels = Proxy


-- Property "gtk-color-palette"
_gtkColorPalette :: Proxy "gtk-color-palette"
_gtkColorPalette = Proxy


-- Property "gtk-color-scheme"
_gtkColorScheme :: Proxy "gtk-color-scheme"
_gtkColorScheme = Proxy


-- Property "gtk-cursor-blink"
_gtkCursorBlink :: Proxy "gtk-cursor-blink"
_gtkCursorBlink = Proxy


-- Property "gtk-cursor-blink-time"
_gtkCursorBlinkTime :: Proxy "gtk-cursor-blink-time"
_gtkCursorBlinkTime = Proxy


-- Property "gtk-cursor-blink-timeout"
_gtkCursorBlinkTimeout :: Proxy "gtk-cursor-blink-timeout"
_gtkCursorBlinkTimeout = Proxy


-- Property "gtk-cursor-theme-name"
_gtkCursorThemeName :: Proxy "gtk-cursor-theme-name"
_gtkCursorThemeName = Proxy


-- Property "gtk-cursor-theme-size"
_gtkCursorThemeSize :: Proxy "gtk-cursor-theme-size"
_gtkCursorThemeSize = Proxy


-- Property "gtk-decoration-layout"
_gtkDecorationLayout :: Proxy "gtk-decoration-layout"
_gtkDecorationLayout = Proxy


-- Property "gtk-dialogs-use-header"
_gtkDialogsUseHeader :: Proxy "gtk-dialogs-use-header"
_gtkDialogsUseHeader = Proxy


-- Property "gtk-dnd-drag-threshold"
_gtkDndDragThreshold :: Proxy "gtk-dnd-drag-threshold"
_gtkDndDragThreshold = Proxy


-- Property "gtk-double-click-distance"
_gtkDoubleClickDistance :: Proxy "gtk-double-click-distance"
_gtkDoubleClickDistance = Proxy


-- Property "gtk-double-click-time"
_gtkDoubleClickTime :: Proxy "gtk-double-click-time"
_gtkDoubleClickTime = Proxy


-- Property "gtk-enable-accels"
_gtkEnableAccels :: Proxy "gtk-enable-accels"
_gtkEnableAccels = Proxy


-- Property "gtk-enable-animations"
_gtkEnableAnimations :: Proxy "gtk-enable-animations"
_gtkEnableAnimations = Proxy


-- Property "gtk-enable-event-sounds"
_gtkEnableEventSounds :: Proxy "gtk-enable-event-sounds"
_gtkEnableEventSounds = Proxy


-- Property "gtk-enable-input-feedback-sounds"
_gtkEnableInputFeedbackSounds :: Proxy "gtk-enable-input-feedback-sounds"
_gtkEnableInputFeedbackSounds = Proxy


-- Property "gtk-enable-mnemonics"
_gtkEnableMnemonics :: Proxy "gtk-enable-mnemonics"
_gtkEnableMnemonics = Proxy


-- Property "gtk-enable-primary-paste"
_gtkEnablePrimaryPaste :: Proxy "gtk-enable-primary-paste"
_gtkEnablePrimaryPaste = Proxy


-- Property "gtk-enable-tooltips"
_gtkEnableTooltips :: Proxy "gtk-enable-tooltips"
_gtkEnableTooltips = Proxy


-- Property "gtk-entry-password-hint-timeout"
_gtkEntryPasswordHintTimeout :: Proxy "gtk-entry-password-hint-timeout"
_gtkEntryPasswordHintTimeout = Proxy


-- Property "gtk-entry-select-on-focus"
_gtkEntrySelectOnFocus :: Proxy "gtk-entry-select-on-focus"
_gtkEntrySelectOnFocus = Proxy


-- Property "gtk-error-bell"
_gtkErrorBell :: Proxy "gtk-error-bell"
_gtkErrorBell = Proxy


-- Property "gtk-fallback-icon-theme"
_gtkFallbackIconTheme :: Proxy "gtk-fallback-icon-theme"
_gtkFallbackIconTheme = Proxy


-- Property "gtk-file-chooser-backend"
_gtkFileChooserBackend :: Proxy "gtk-file-chooser-backend"
_gtkFileChooserBackend = Proxy


-- Property "gtk-font-name"
_gtkFontName :: Proxy "gtk-font-name"
_gtkFontName = Proxy


-- Property "gtk-fontconfig-timestamp"
_gtkFontconfigTimestamp :: Proxy "gtk-fontconfig-timestamp"
_gtkFontconfigTimestamp = Proxy


-- Property "gtk-icon-sizes"
_gtkIconSizes :: Proxy "gtk-icon-sizes"
_gtkIconSizes = Proxy


-- Property "gtk-icon-theme-name"
_gtkIconThemeName :: Proxy "gtk-icon-theme-name"
_gtkIconThemeName = Proxy


-- Property "gtk-im-module"
_gtkImModule :: Proxy "gtk-im-module"
_gtkImModule = Proxy


-- Property "gtk-im-preedit-style"
_gtkImPreeditStyle :: Proxy "gtk-im-preedit-style"
_gtkImPreeditStyle = Proxy


-- Property "gtk-im-status-style"
_gtkImStatusStyle :: Proxy "gtk-im-status-style"
_gtkImStatusStyle = Proxy


-- Property "gtk-key-theme-name"
_gtkKeyThemeName :: Proxy "gtk-key-theme-name"
_gtkKeyThemeName = Proxy


-- Property "gtk-keynav-cursor-only"
_gtkKeynavCursorOnly :: Proxy "gtk-keynav-cursor-only"
_gtkKeynavCursorOnly = Proxy


-- Property "gtk-keynav-wrap-around"
_gtkKeynavWrapAround :: Proxy "gtk-keynav-wrap-around"
_gtkKeynavWrapAround = Proxy


-- Property "gtk-label-select-on-focus"
_gtkLabelSelectOnFocus :: Proxy "gtk-label-select-on-focus"
_gtkLabelSelectOnFocus = Proxy


-- Property "gtk-long-press-time"
_gtkLongPressTime :: Proxy "gtk-long-press-time"
_gtkLongPressTime = Proxy


-- Property "gtk-menu-bar-accel"
_gtkMenuBarAccel :: Proxy "gtk-menu-bar-accel"
_gtkMenuBarAccel = Proxy


-- Property "gtk-menu-bar-popup-delay"
_gtkMenuBarPopupDelay :: Proxy "gtk-menu-bar-popup-delay"
_gtkMenuBarPopupDelay = Proxy


-- Property "gtk-menu-images"
_gtkMenuImages :: Proxy "gtk-menu-images"
_gtkMenuImages = Proxy


-- Property "gtk-menu-popdown-delay"
_gtkMenuPopdownDelay :: Proxy "gtk-menu-popdown-delay"
_gtkMenuPopdownDelay = Proxy


-- Property "gtk-menu-popup-delay"
_gtkMenuPopupDelay :: Proxy "gtk-menu-popup-delay"
_gtkMenuPopupDelay = Proxy


-- Property "gtk-modules"
_gtkModules :: Proxy "gtk-modules"
_gtkModules = Proxy


-- Property "gtk-primary-button-warps-slider"
_gtkPrimaryButtonWarpsSlider :: Proxy "gtk-primary-button-warps-slider"
_gtkPrimaryButtonWarpsSlider = Proxy


-- Property "gtk-print-backends"
_gtkPrintBackends :: Proxy "gtk-print-backends"
_gtkPrintBackends = Proxy


-- Property "gtk-print-preview-command"
_gtkPrintPreviewCommand :: Proxy "gtk-print-preview-command"
_gtkPrintPreviewCommand = Proxy


-- Property "gtk-recent-files-enabled"
_gtkRecentFilesEnabled :: Proxy "gtk-recent-files-enabled"
_gtkRecentFilesEnabled = Proxy


-- Property "gtk-recent-files-limit"
_gtkRecentFilesLimit :: Proxy "gtk-recent-files-limit"
_gtkRecentFilesLimit = Proxy


-- Property "gtk-recent-files-max-age"
_gtkRecentFilesMaxAge :: Proxy "gtk-recent-files-max-age"
_gtkRecentFilesMaxAge = Proxy


-- Property "gtk-scrolled-window-placement"
_gtkScrolledWindowPlacement :: Proxy "gtk-scrolled-window-placement"
_gtkScrolledWindowPlacement = Proxy


-- Property "gtk-shell-shows-app-menu"
_gtkShellShowsAppMenu :: Proxy "gtk-shell-shows-app-menu"
_gtkShellShowsAppMenu = Proxy


-- Property "gtk-shell-shows-desktop"
_gtkShellShowsDesktop :: Proxy "gtk-shell-shows-desktop"
_gtkShellShowsDesktop = Proxy


-- Property "gtk-shell-shows-menubar"
_gtkShellShowsMenubar :: Proxy "gtk-shell-shows-menubar"
_gtkShellShowsMenubar = Proxy


-- Property "gtk-show-input-method-menu"
_gtkShowInputMethodMenu :: Proxy "gtk-show-input-method-menu"
_gtkShowInputMethodMenu = Proxy


-- Property "gtk-show-unicode-menu"
_gtkShowUnicodeMenu :: Proxy "gtk-show-unicode-menu"
_gtkShowUnicodeMenu = Proxy


-- Property "gtk-sound-theme-name"
_gtkSoundThemeName :: Proxy "gtk-sound-theme-name"
_gtkSoundThemeName = Proxy


-- Property "gtk-split-cursor"
_gtkSplitCursor :: Proxy "gtk-split-cursor"
_gtkSplitCursor = Proxy


-- Property "gtk-theme-name"
_gtkThemeName :: Proxy "gtk-theme-name"
_gtkThemeName = Proxy


-- Property "gtk-timeout-expand"
_gtkTimeoutExpand :: Proxy "gtk-timeout-expand"
_gtkTimeoutExpand = Proxy


-- Property "gtk-timeout-initial"
_gtkTimeoutInitial :: Proxy "gtk-timeout-initial"
_gtkTimeoutInitial = Proxy


-- Property "gtk-timeout-repeat"
_gtkTimeoutRepeat :: Proxy "gtk-timeout-repeat"
_gtkTimeoutRepeat = Proxy


-- Property "gtk-titlebar-double-click"
_gtkTitlebarDoubleClick :: Proxy "gtk-titlebar-double-click"
_gtkTitlebarDoubleClick = Proxy


-- Property "gtk-titlebar-middle-click"
_gtkTitlebarMiddleClick :: Proxy "gtk-titlebar-middle-click"
_gtkTitlebarMiddleClick = Proxy


-- Property "gtk-titlebar-right-click"
_gtkTitlebarRightClick :: Proxy "gtk-titlebar-right-click"
_gtkTitlebarRightClick = Proxy


-- Property "gtk-toolbar-icon-size"
_gtkToolbarIconSize :: Proxy "gtk-toolbar-icon-size"
_gtkToolbarIconSize = Proxy


-- Property "gtk-toolbar-style"
_gtkToolbarStyle :: Proxy "gtk-toolbar-style"
_gtkToolbarStyle = Proxy


-- Property "gtk-tooltip-browse-mode-timeout"
_gtkTooltipBrowseModeTimeout :: Proxy "gtk-tooltip-browse-mode-timeout"
_gtkTooltipBrowseModeTimeout = Proxy


-- Property "gtk-tooltip-browse-timeout"
_gtkTooltipBrowseTimeout :: Proxy "gtk-tooltip-browse-timeout"
_gtkTooltipBrowseTimeout = Proxy


-- Property "gtk-tooltip-timeout"
_gtkTooltipTimeout :: Proxy "gtk-tooltip-timeout"
_gtkTooltipTimeout = Proxy


-- Property "gtk-touchscreen-mode"
_gtkTouchscreenMode :: Proxy "gtk-touchscreen-mode"
_gtkTouchscreenMode = Proxy


-- Property "gtk-visible-focus"
_gtkVisibleFocus :: Proxy "gtk-visible-focus"
_gtkVisibleFocus = Proxy


-- Property "gtk-xft-antialias"
_gtkXftAntialias :: Proxy "gtk-xft-antialias"
_gtkXftAntialias = Proxy


-- Property "gtk-xft-dpi"
_gtkXftDpi :: Proxy "gtk-xft-dpi"
_gtkXftDpi = Proxy


-- Property "gtk-xft-hinting"
_gtkXftHinting :: Proxy "gtk-xft-hinting"
_gtkXftHinting = Proxy


-- Property "gtk-xft-hintstyle"
_gtkXftHintstyle :: Proxy "gtk-xft-hintstyle"
_gtkXftHintstyle = Proxy


-- Property "gtk-xft-rgba"
_gtkXftRgba :: Proxy "gtk-xft-rgba"
_gtkXftRgba = Proxy


-- Property "hadjustment"
_hadjustment :: Proxy "hadjustment"
_hadjustment = Proxy


-- Property "halign"
_halign :: Proxy "halign"
_halign = Proxy


-- Property "handle-position"
_handlePosition :: Proxy "handle-position"
_handlePosition = Proxy


-- Property "has-alpha"
_hasAlpha :: Proxy "has-alpha"
_hasAlpha = Proxy


-- Property "has-default"
_hasDefault :: Proxy "has-default"
_hasDefault = Proxy


-- Property "has-depth-buffer"
_hasDepthBuffer :: Proxy "has-depth-buffer"
_hasDepthBuffer = Proxy


-- Property "has-entry"
_hasEntry :: Proxy "has-entry"
_hasEntry = Proxy


-- Property "has-focus"
_hasFocus :: Proxy "has-focus"
_hasFocus = Proxy


-- Property "has-frame"
_hasFrame :: Proxy "has-frame"
_hasFrame = Proxy


-- Property "has-opacity-control"
_hasOpacityControl :: Proxy "has-opacity-control"
_hasOpacityControl = Proxy


-- Property "has-origin"
_hasOrigin :: Proxy "has-origin"
_hasOrigin = Proxy


-- Property "has-palette"
_hasPalette :: Proxy "has-palette"
_hasPalette = Proxy


-- Property "has-resize-grip"
_hasResizeGrip :: Proxy "has-resize-grip"
_hasResizeGrip = Proxy


-- Property "has-selection"
_hasSelection :: Proxy "has-selection"
_hasSelection = Proxy


-- Property "has-stencil-buffer"
_hasStencilBuffer :: Proxy "has-stencil-buffer"
_hasStencilBuffer = Proxy


-- Property "has-subtitle"
_hasSubtitle :: Proxy "has-subtitle"
_hasSubtitle = Proxy


-- Property "has-tooltip"
_hasTooltip :: Proxy "has-tooltip"
_hasTooltip = Proxy


-- Property "has-toplevel-focus"
_hasToplevelFocus :: Proxy "has-toplevel-focus"
_hasToplevelFocus = Proxy


-- Property "hash"
_hash :: Proxy "hash"
_hash = Proxy


-- Property "head"
_head :: Proxy "head"
_head = Proxy


-- Property "header-relief"
_headerRelief :: Proxy "header-relief"
_headerRelief = Proxy


-- Property "headers"
_headers :: Proxy "headers"
_headers = Proxy


-- Property "headers-clickable"
_headersClickable :: Proxy "headers-clickable"
_headersClickable = Proxy


-- Property "headers-visible"
_headersVisible :: Proxy "headers-visible"
_headersVisible = Proxy


-- Property "heading"
_heading :: Proxy "heading"
_heading = Proxy


-- Property "height"
_height :: Proxy "height"
_height = Proxy


-- Property "height-request"
_heightRequest :: Proxy "height-request"
_heightRequest = Proxy


-- Property "help-button"
_helpButton :: Proxy "help-button"
_helpButton = Proxy


-- Property "hexpand"
_hexpand :: Proxy "hexpand"
_hexpand = Proxy


-- Property "hexpand-set"
_hexpandSet :: Proxy "hexpand-set"
_hexpandSet = Proxy


-- Property "hhomogeneous"
_hhomogeneous :: Proxy "hhomogeneous"
_hhomogeneous = Proxy


-- Property "hidden"
_hidden :: Proxy "hidden"
_hidden = Proxy


-- Property "hide-if-empty"
_hideIfEmpty :: Proxy "hide-if-empty"
_hideIfEmpty = Proxy


-- Property "hide-titlebar-when-maximized"
_hideTitlebarWhenMaximized :: Proxy "hide-titlebar-when-maximized"
_hideTitlebarWhenMaximized = Proxy


-- Property "history"
_history :: Proxy "history"
_history = Proxy


-- Property "homogeneous"
_homogeneous :: Proxy "homogeneous"
_homogeneous = Proxy


-- Property "horizontal-scrollbar-policy"
_horizontalScrollbarPolicy :: Proxy "horizontal-scrollbar-policy"
_horizontalScrollbarPolicy = Proxy


-- Property "host"
_host :: Proxy "host"
_host = Proxy


-- Property "hostname"
_hostname :: Proxy "hostname"
_hostname = Proxy


-- Property "hover-expand"
_hoverExpand :: Proxy "hover-expand"
_hoverExpand = Proxy


-- Property "hover-selection"
_hoverSelection :: Proxy "hover-selection"
_hoverSelection = Proxy


-- Property "href"
_href :: Proxy "href"
_href = Proxy


-- Property "hreflang"
_hreflang :: Proxy "hreflang"
_hreflang = Proxy


-- Property "hscroll-policy"
_hscrollPolicy :: Proxy "hscroll-policy"
_hscrollPolicy = Proxy


-- Property "hscrollbar-policy"
_hscrollbarPolicy :: Proxy "hscrollbar-policy"
_hscrollbarPolicy = Proxy


-- Property "hspace"
_hspace :: Proxy "hspace"
_hspace = Proxy


-- Property "html-for"
_htmlFor :: Proxy "html-for"
_htmlFor = Proxy


-- Property "html5-local-storage-database-path"
_html5LocalStorageDatabasePath :: Proxy "html5-local-storage-database-path"
_html5LocalStorageDatabasePath = Proxy


-- Property "http-equiv"
_httpEquiv :: Proxy "http-equiv"
_httpEquiv = Proxy


-- Property "icon"
_icon :: Proxy "icon"
_icon = Proxy


-- Property "icon-name"
_iconName :: Proxy "icon-name"
_iconName = Proxy


-- Property "icon-set"
_iconSet :: Proxy "icon-set"
_iconSet = Proxy


-- Property "icon-size"
_iconSize :: Proxy "icon-size"
_iconSize = Proxy


-- Property "icon-size-set"
_iconSizeSet :: Proxy "icon-size-set"
_iconSizeSet = Proxy


-- Property "icon-uri"
_iconUri :: Proxy "icon-uri"
_iconUri = Proxy


-- Property "icon-widget"
_iconWidget :: Proxy "icon-widget"
_iconWidget = Proxy


-- Property "iconic"
_iconic :: Proxy "iconic"
_iconic = Proxy


-- Property "icons"
_icons :: Proxy "icons"
_icons = Proxy


-- Property "id"
_id :: Proxy "id"
_id = Proxy


-- Property "id-column"
_idColumn :: Proxy "id-column"
_idColumn = Proxy


-- Property "identifier"
_identifier :: Proxy "identifier"
_identifier = Proxy


-- Property "ignore-hidden"
_ignoreHidden :: Proxy "ignore-hidden"
_ignoreHidden = Proxy


-- Property "im-context"
_imContext :: Proxy "im-context"
_imContext = Proxy


-- Property "im-module"
_imModule :: Proxy "im-module"
_imModule = Proxy


-- Property "image"
_image :: Proxy "image"
_image = Proxy


-- Property "image-position"
_imagePosition :: Proxy "image-position"
_imagePosition = Proxy


-- Property "image-uri"
_imageUri :: Proxy "image-uri"
_imageUri = Proxy


-- Property "images"
_images :: Proxy "images"
_images = Proxy


-- Property "implementation"
_implementation :: Proxy "implementation"
_implementation = Proxy


-- Property "inconsistent"
_inconsistent :: Proxy "inconsistent"
_inconsistent = Proxy


-- Property "incremental"
_incremental :: Proxy "incremental"
_incremental = Proxy


-- Property "indent"
_indent :: Proxy "indent"
_indent = Proxy


-- Property "indent-set"
_indentSet :: Proxy "indent-set"
_indentSet = Proxy


-- Property "indeterminate"
_indeterminate :: Proxy "indeterminate"
_indeterminate = Proxy


-- Property "index"
_index :: Proxy "index"
_index = Proxy


-- Property "indicator-size"
_indicatorSize :: Proxy "indicator-size"
_indicatorSize = Proxy


-- Property "initial-scale-factor"
_initialScaleFactor :: Proxy "initial-scale-factor"
_initialScaleFactor = Proxy


-- Property "inline-completion"
_inlineCompletion :: Proxy "inline-completion"
_inlineCompletion = Proxy


-- Property "inline-selection"
_inlineSelection :: Proxy "inline-selection"
_inlineSelection = Proxy


-- Property "inner-border"
_innerBorder :: Proxy "inner-border"
_innerBorder = Proxy


-- Property "inner-height"
_innerHeight :: Proxy "inner-height"
_innerHeight = Proxy


-- Property "inner-html"
_innerHtml :: Proxy "inner-html"
_innerHtml = Proxy


-- Property "inner-node"
_innerNode :: Proxy "inner-node"
_innerNode = Proxy


-- Property "inner-text"
_innerText :: Proxy "inner-text"
_innerText = Proxy


-- Property "inner-width"
_innerWidth :: Proxy "inner-width"
_innerWidth = Proxy


-- Property "input-encoding"
_inputEncoding :: Proxy "input-encoding"
_inputEncoding = Proxy


-- Property "input-hints"
_inputHints :: Proxy "input-hints"
_inputHints = Proxy


-- Property "input-purpose"
_inputPurpose :: Proxy "input-purpose"
_inputPurpose = Proxy


-- Property "inspected-uri"
_inspectedUri :: Proxy "inspected-uri"
_inspectedUri = Proxy


-- Property "internal-subset"
_internalSubset :: Proxy "internal-subset"
_internalSubset = Proxy


-- Property "invalid-iterator-state"
_invalidIteratorState :: Proxy "invalid-iterator-state"
_invalidIteratorState = Proxy


-- Property "inverted"
_inverted :: Proxy "inverted"
_inverted = Proxy


-- Property "invisible"
_invisible :: Proxy "invisible"
_invisible = Proxy


-- Property "invisible-char"
_invisibleChar :: Proxy "invisible-char"
_invisibleChar = Proxy


-- Property "invisible-char-set"
_invisibleCharSet :: Proxy "invisible-char-set"
_invisibleCharSet = Proxy


-- Property "invisible-set"
_invisibleSet :: Proxy "invisible-set"
_invisibleSet = Proxy


-- Property "is-active"
_isActive :: Proxy "is-active"
_isActive = Proxy


-- Property "is-collapsed"
_isCollapsed :: Proxy "is-collapsed"
_isCollapsed = Proxy


-- Property "is-content-editable"
_isContentEditable :: Proxy "is-content-editable"
_isContentEditable = Proxy


-- Property "is-expanded"
_isExpanded :: Proxy "is-expanded"
_isExpanded = Proxy


-- Property "is-expander"
_isExpander :: Proxy "is-expander"
_isExpander = Proxy


-- Property "is-focus"
_isFocus :: Proxy "is-focus"
_isFocus = Proxy


-- Property "is-id"
_isId :: Proxy "is-id"
_isId = Proxy


-- Property "is-important"
_isImportant :: Proxy "is-important"
_isImportant = Proxy


-- Property "is-locked"
_isLocked :: Proxy "is-locked"
_isLocked = Proxy


-- Property "is-map"
_isMap :: Proxy "is-map"
_isMap = Proxy


-- Property "is-maximized"
_isMaximized :: Proxy "is-maximized"
_isMaximized = Proxy


-- Property "is-showing"
_isShowing :: Proxy "is-showing"
_isShowing = Proxy


-- Property "item-orientation"
_itemOrientation :: Proxy "item-orientation"
_itemOrientation = Proxy


-- Property "item-padding"
_itemPadding :: Proxy "item-padding"
_itemPadding = Proxy


-- Property "item-width"
_itemWidth :: Proxy "item-width"
_itemWidth = Proxy


-- Property "javascript-can-access-clipboard"
_javascriptCanAccessClipboard :: Proxy "javascript-can-access-clipboard"
_javascriptCanAccessClipboard = Proxy


-- Property "javascript-can-open-windows-automatically"
_javascriptCanOpenWindowsAutomatically :: Proxy "javascript-can-open-windows-automatically"
_javascriptCanOpenWindowsAutomatically = Proxy


-- Property "javascript-profiling-enabled"
_javascriptProfilingEnabled :: Proxy "javascript-profiling-enabled"
_javascriptProfilingEnabled = Proxy


-- Property "job-name"
_jobName :: Proxy "job-name"
_jobName = Proxy


-- Property "js-heap-size-limit"
_jsHeapSizeLimit :: Proxy "js-heap-size-limit"
_jsHeapSizeLimit = Proxy


-- Property "justification"
_justification :: Proxy "justification"
_justification = Proxy


-- Property "justification-set"
_justificationSet :: Proxy "justification-set"
_justificationSet = Proxy


-- Property "justify"
_justify :: Proxy "justify"
_justify = Proxy


-- Property "key-code"
_keyCode :: Proxy "key-code"
_keyCode = Proxy


-- Property "key-identifier"
_keyIdentifier :: Proxy "key-identifier"
_keyIdentifier = Proxy


-- Property "key-location"
_keyLocation :: Proxy "key-location"
_keyLocation = Proxy


-- Property "keycode"
_keycode :: Proxy "keycode"
_keycode = Proxy


-- Property "keytype"
_keytype :: Proxy "keytype"
_keytype = Proxy


-- Property "kind"
_kind :: Proxy "kind"
_kind = Proxy


-- Property "kinetic-scrolling"
_kineticScrolling :: Proxy "kinetic-scrolling"
_kineticScrolling = Proxy


-- Property "label"
_label :: Proxy "label"
_label = Proxy


-- Property "label-fill"
_labelFill :: Proxy "label-fill"
_labelFill = Proxy


-- Property "label-widget"
_labelWidget :: Proxy "label-widget"
_labelWidget = Proxy


-- Property "label-xalign"
_labelXalign :: Proxy "label-xalign"
_labelXalign = Proxy


-- Property "label-yalign"
_labelYalign :: Proxy "label-yalign"
_labelYalign = Proxy


-- Property "labels"
_labels :: Proxy "labels"
_labels = Proxy


-- Property "lang"
_lang :: Proxy "lang"
_lang = Proxy


-- Property "language"
_language :: Proxy "language"
_language = Proxy


-- Property "language-set"
_languageSet :: Proxy "language-set"
_languageSet = Proxy


-- Property "last-child"
_lastChild :: Proxy "last-child"
_lastChild = Proxy


-- Property "last-element-child"
_lastElementChild :: Proxy "last-element-child"
_lastElementChild = Proxy


-- Property "last-modified"
_lastModified :: Proxy "last-modified"
_lastModified = Proxy


-- Property "last-visited-time"
_lastVisitedTime :: Proxy "last-visited-time"
_lastVisitedTime = Proxy


-- Property "layer-x"
_layerX :: Proxy "layer-x"
_layerX = Proxy


-- Property "layer-y"
_layerY :: Proxy "layer-y"
_layerY = Proxy


-- Property "layout-style"
_layoutStyle :: Proxy "layout-style"
_layoutStyle = Proxy


-- Property "left-gravity"
_leftGravity :: Proxy "left-gravity"
_leftGravity = Proxy


-- Property "left-margin"
_leftMargin :: Proxy "left-margin"
_leftMargin = Proxy


-- Property "left-margin-set"
_leftMarginSet :: Proxy "left-margin-set"
_leftMarginSet = Proxy


-- Property "left-padding"
_leftPadding :: Proxy "left-padding"
_leftPadding = Proxy


-- Property "length"
_length :: Proxy "length"
_length = Proxy


-- Property "letter-spacing"
_letterSpacing :: Proxy "letter-spacing"
_letterSpacing = Proxy


-- Property "letter-spacing-set"
_letterSpacingSet :: Proxy "letter-spacing-set"
_letterSpacingSet = Proxy


-- Property "level"
_level :: Proxy "level"
_level = Proxy


-- Property "level-indentation"
_levelIndentation :: Proxy "level-indentation"
_levelIndentation = Proxy


-- Property "license"
_license :: Proxy "license"
_license = Proxy


-- Property "license-type"
_licenseType :: Proxy "license-type"
_licenseType = Proxy


-- Property "limit"
_limit :: Proxy "limit"
_limit = Proxy


-- Property "line"
_line :: Proxy "line"
_line = Proxy


-- Property "lines"
_lines :: Proxy "lines"
_lines = Proxy


-- Property "link"
_link :: Proxy "link"
_link = Proxy


-- Property "link-color"
_linkColor :: Proxy "link-color"
_linkColor = Proxy


-- Property "link-uri"
_linkUri :: Proxy "link-uri"
_linkUri = Proxy


-- Property "links"
_links :: Proxy "links"
_links = Proxy


-- Property "list"
_list :: Proxy "list"
_list = Proxy


-- Property "load-event-end"
_loadEventEnd :: Proxy "load-event-end"
_loadEventEnd = Proxy


-- Property "load-event-start"
_loadEventStart :: Proxy "load-event-start"
_loadEventStart = Proxy


-- Property "load-status"
_loadStatus :: Proxy "load-status"
_loadStatus = Proxy


-- Property "local-name"
_localName :: Proxy "local-name"
_localName = Proxy


-- Property "local-only"
_localOnly :: Proxy "local-only"
_localOnly = Proxy


-- Property "local-storage"
_localStorage :: Proxy "local-storage"
_localStorage = Proxy


-- Property "location"
_location :: Proxy "location"
_location = Proxy


-- Property "locationbar"
_locationbar :: Proxy "locationbar"
_locationbar = Proxy


-- Property "locationbar-visible"
_locationbarVisible :: Proxy "locationbar-visible"
_locationbarVisible = Proxy


-- Property "logo"
_logo :: Proxy "logo"
_logo = Proxy


-- Property "logo-icon-name"
_logoIconName :: Proxy "logo-icon-name"
_logoIconName = Proxy


-- Property "long-desc"
_longDesc :: Proxy "long-desc"
_longDesc = Proxy


-- Property "loop"
_loop :: Proxy "loop"
_loop = Proxy


-- Property "lower"
_lower :: Proxy "lower"
_lower = Proxy


-- Property "lower-stepper-sensitivity"
_lowerStepperSensitivity :: Proxy "lower-stepper-sensitivity"
_lowerStepperSensitivity = Proxy


-- Property "lowsrc"
_lowsrc :: Proxy "lowsrc"
_lowsrc = Proxy


-- Property "manifest"
_manifest :: Proxy "manifest"
_manifest = Proxy


-- Property "margin"
_margin :: Proxy "margin"
_margin = Proxy


-- Property "margin-bottom"
_marginBottom :: Proxy "margin-bottom"
_marginBottom = Proxy


-- Property "margin-end"
_marginEnd :: Proxy "margin-end"
_marginEnd = Proxy


-- Property "margin-height"
_marginHeight :: Proxy "margin-height"
_marginHeight = Proxy


-- Property "margin-left"
_marginLeft :: Proxy "margin-left"
_marginLeft = Proxy


-- Property "margin-right"
_marginRight :: Proxy "margin-right"
_marginRight = Proxy


-- Property "margin-start"
_marginStart :: Proxy "margin-start"
_marginStart = Proxy


-- Property "margin-top"
_marginTop :: Proxy "margin-top"
_marginTop = Proxy


-- Property "margin-width"
_marginWidth :: Proxy "margin-width"
_marginWidth = Proxy


-- Property "markup"
_markup :: Proxy "markup"
_markup = Proxy


-- Property "markup-column"
_markupColumn :: Proxy "markup-column"
_markupColumn = Proxy


-- Property "matches"
_matches :: Proxy "matches"
_matches = Proxy


-- Property "max"
_max :: Proxy "max"
_max = Proxy


-- Property "max-children-per-line"
_maxChildrenPerLine :: Proxy "max-children-per-line"
_maxChildrenPerLine = Proxy


-- Property "max-length"
_maxLength :: Proxy "max-length"
_maxLength = Proxy


-- Property "max-position"
_maxPosition :: Proxy "max-position"
_maxPosition = Proxy


-- Property "max-value"
_maxValue :: Proxy "max-value"
_maxValue = Proxy


-- Property "max-width"
_maxWidth :: Proxy "max-width"
_maxWidth = Proxy


-- Property "max-width-chars"
_maxWidthChars :: Proxy "max-width-chars"
_maxWidthChars = Proxy


-- Property "maximum-scale-factor"
_maximumScaleFactor :: Proxy "maximum-scale-factor"
_maximumScaleFactor = Proxy


-- Property "media"
_media :: Proxy "media"
_media = Proxy


-- Property "media-group"
_mediaGroup :: Proxy "media-group"
_mediaGroup = Proxy


-- Property "media-playback-allows-inline"
_mediaPlaybackAllowsInline :: Proxy "media-playback-allows-inline"
_mediaPlaybackAllowsInline = Proxy


-- Property "media-playback-requires-user-gesture"
_mediaPlaybackRequiresUserGesture :: Proxy "media-playback-requires-user-gesture"
_mediaPlaybackRequiresUserGesture = Proxy


-- Property "media-text"
_mediaText :: Proxy "media-text"
_mediaText = Proxy


-- Property "media-uri"
_mediaUri :: Proxy "media-uri"
_mediaUri = Proxy


-- Property "menu"
_menu :: Proxy "menu"
_menu = Proxy


-- Property "menu-model"
_menuModel :: Proxy "menu-model"
_menuModel = Proxy


-- Property "menu-name"
_menuName :: Proxy "menu-name"
_menuName = Proxy


-- Property "menubar"
_menubar :: Proxy "menubar"
_menubar = Proxy


-- Property "menubar-visible"
_menubarVisible :: Proxy "menubar-visible"
_menubarVisible = Proxy


-- Property "message"
_message :: Proxy "message"
_message = Proxy


-- Property "message-area"
_messageArea :: Proxy "message-area"
_messageArea = Proxy


-- Property "message-type"
_messageType :: Proxy "message-type"
_messageType = Proxy


-- Property "meta-key"
_metaKey :: Proxy "meta-key"
_metaKey = Proxy


-- Property "method"
_method :: Proxy "method"
_method = Proxy


-- Property "mime-type"
_mimeType :: Proxy "mime-type"
_mimeType = Proxy


-- Property "mime-types"
_mimeTypes :: Proxy "mime-types"
_mimeTypes = Proxy


-- Property "min"
_min :: Proxy "min"
_min = Proxy


-- Property "min-children-per-line"
_minChildrenPerLine :: Proxy "min-children-per-line"
_minChildrenPerLine = Proxy


-- Property "min-content-height"
_minContentHeight :: Proxy "min-content-height"
_minContentHeight = Proxy


-- Property "min-content-width"
_minContentWidth :: Proxy "min-content-width"
_minContentWidth = Proxy


-- Property "min-position"
_minPosition :: Proxy "min-position"
_minPosition = Proxy


-- Property "min-value"
_minValue :: Proxy "min-value"
_minValue = Proxy


-- Property "min-width"
_minWidth :: Proxy "min-width"
_minWidth = Proxy


-- Property "minimum-font-size"
_minimumFontSize :: Proxy "minimum-font-size"
_minimumFontSize = Proxy


-- Property "minimum-height"
_minimumHeight :: Proxy "minimum-height"
_minimumHeight = Proxy


-- Property "minimum-key-length"
_minimumKeyLength :: Proxy "minimum-key-length"
_minimumKeyLength = Proxy


-- Property "minimum-logical-font-size"
_minimumLogicalFontSize :: Proxy "minimum-logical-font-size"
_minimumLogicalFontSize = Proxy


-- Property "minimum-scale-factor"
_minimumScaleFactor :: Proxy "minimum-scale-factor"
_minimumScaleFactor = Proxy


-- Property "minimum-width"
_minimumWidth :: Proxy "minimum-width"
_minimumWidth = Proxy


-- Property "mnemonic-keyval"
_mnemonicKeyval :: Proxy "mnemonic-keyval"
_mnemonicKeyval = Proxy


-- Property "mnemonic-widget"
_mnemonicWidget :: Proxy "mnemonic-widget"
_mnemonicWidget = Proxy


-- Property "mnemonics-visible"
_mnemonicsVisible :: Proxy "mnemonics-visible"
_mnemonicsVisible = Proxy


-- Property "modal"
_modal :: Proxy "modal"
_modal = Proxy


-- Property "mode"
_mode :: Proxy "mode"
_mode = Proxy


-- Property "model"
_model :: Proxy "model"
_model = Proxy


-- Property "modifier-mask"
_modifierMask :: Proxy "modifier-mask"
_modifierMask = Proxy


-- Property "modifier-state"
_modifierState :: Proxy "modifier-state"
_modifierState = Proxy


-- Property "monitor"
_monitor :: Proxy "monitor"
_monitor = Proxy


-- Property "monospace"
_monospace :: Proxy "monospace"
_monospace = Proxy


-- Property "monospace-font-family"
_monospaceFontFamily :: Proxy "monospace-font-family"
_monospaceFontFamily = Proxy


-- Property "month"
_month :: Proxy "month"
_month = Proxy


-- Property "multiple"
_multiple :: Proxy "multiple"
_multiple = Proxy


-- Property "muted"
_muted :: Proxy "muted"
_muted = Proxy


-- Property "n-columns"
_nColumns :: Proxy "n-columns"
_nColumns = Proxy


-- Property "n-pages"
_nPages :: Proxy "n-pages"
_nPages = Proxy


-- Property "n-pages-to-print"
_nPagesToPrint :: Proxy "n-pages-to-print"
_nPagesToPrint = Proxy


-- Property "n-points"
_nPoints :: Proxy "n-points"
_nPoints = Proxy


-- Property "n-rows"
_nRows :: Proxy "n-rows"
_nRows = Proxy


-- Property "name"
_name :: Proxy "name"
_name = Proxy


-- Property "names"
_names :: Proxy "names"
_names = Proxy


-- Property "namespace-uri"
_namespaceUri :: Proxy "namespace-uri"
_namespaceUri = Proxy


-- Property "natural-height"
_naturalHeight :: Proxy "natural-height"
_naturalHeight = Proxy


-- Property "natural-width"
_naturalWidth :: Proxy "natural-width"
_naturalWidth = Proxy


-- Property "navigation"
_navigation :: Proxy "navigation"
_navigation = Proxy


-- Property "navigation-start"
_navigationStart :: Proxy "navigation-start"
_navigationStart = Proxy


-- Property "navigator"
_navigator :: Proxy "navigator"
_navigator = Proxy


-- Property "network-request"
_networkRequest :: Proxy "network-request"
_networkRequest = Proxy


-- Property "network-response"
_networkResponse :: Proxy "network-response"
_networkResponse = Proxy


-- Property "network-state"
_networkState :: Proxy "network-state"
_networkState = Proxy


-- Property "next-element-sibling"
_nextElementSibling :: Proxy "next-element-sibling"
_nextElementSibling = Proxy


-- Property "next-sibling"
_nextSibling :: Proxy "next-sibling"
_nextSibling = Proxy


-- Property "no-href"
_noHref :: Proxy "no-href"
_noHref = Proxy


-- Property "no-month-change"
_noMonthChange :: Proxy "no-month-change"
_noMonthChange = Proxy


-- Property "no-resize"
_noResize :: Proxy "no-resize"
_noResize = Proxy


-- Property "no-shade"
_noShade :: Proxy "no-shade"
_noShade = Proxy


-- Property "no-show-all"
_noShowAll :: Proxy "no-show-all"
_noShowAll = Proxy


-- Property "no-validate"
_noValidate :: Proxy "no-validate"
_noValidate = Proxy


-- Property "no-wrap"
_noWrap :: Proxy "no-wrap"
_noWrap = Proxy


-- Property "node-name"
_nodeName :: Proxy "node-name"
_nodeName = Proxy


-- Property "node-type"
_nodeType :: Proxy "node-type"
_nodeType = Proxy


-- Property "node-value"
_nodeValue :: Proxy "node-value"
_nodeValue = Proxy


-- Property "nonce"
_nonce :: Proxy "nonce"
_nonce = Proxy


-- Property "notations"
_notations :: Proxy "notations"
_notations = Proxy


-- Property "number-value"
_numberValue :: Proxy "number-value"
_numberValue = Proxy


-- Property "numeric"
_numeric :: Proxy "numeric"
_numeric = Proxy


-- Property "obey-child"
_obeyChild :: Proxy "obey-child"
_obeyChild = Proxy


-- Property "object"
_object :: Proxy "object"
_object = Proxy


-- Property "offscreen-buffering"
_offscreenBuffering :: Proxy "offscreen-buffering"
_offscreenBuffering = Proxy


-- Property "offset-height"
_offsetHeight :: Proxy "offset-height"
_offsetHeight = Proxy


-- Property "offset-left"
_offsetLeft :: Proxy "offset-left"
_offsetLeft = Proxy


-- Property "offset-parent"
_offsetParent :: Proxy "offset-parent"
_offsetParent = Proxy


-- Property "offset-top"
_offsetTop :: Proxy "offset-top"
_offsetTop = Proxy


-- Property "offset-width"
_offsetWidth :: Proxy "offset-width"
_offsetWidth = Proxy


-- Property "offset-x"
_offsetX :: Proxy "offset-x"
_offsetX = Proxy


-- Property "offset-y"
_offsetY :: Proxy "offset-y"
_offsetY = Proxy


-- Property "ok-button"
_okButton :: Proxy "ok-button"
_okButton = Proxy


-- Property "on-line"
_onLine :: Proxy "on-line"
_onLine = Proxy


-- Property "opacity"
_opacity :: Proxy "opacity"
_opacity = Proxy


-- Property "open"
_open :: Proxy "open"
_open = Proxy


-- Property "open-flags"
_openFlags :: Proxy "open-flags"
_openFlags = Proxy


-- Property "opener"
_opener :: Proxy "opener"
_opener = Proxy


-- Property "options"
_options :: Proxy "options"
_options = Proxy


-- Property "orientation"
_orientation :: Proxy "orientation"
_orientation = Proxy


-- Property "origin"
_origin :: Proxy "origin"
_origin = Proxy


-- Property "original-uri"
_originalUri :: Proxy "original-uri"
_originalUri = Proxy


-- Property "outer-height"
_outerHeight :: Proxy "outer-height"
_outerHeight = Proxy


-- Property "outer-html"
_outerHtml :: Proxy "outer-html"
_outerHtml = Proxy


-- Property "outer-text"
_outerText :: Proxy "outer-text"
_outerText = Proxy


-- Property "outer-width"
_outerWidth :: Proxy "outer-width"
_outerWidth = Proxy


-- Property "overlay-scrolling"
_overlayScrolling :: Proxy "overlay-scrolling"
_overlayScrolling = Proxy


-- Property "overset"
_overset :: Proxy "overset"
_overset = Proxy


-- Property "overwrite"
_overwrite :: Proxy "overwrite"
_overwrite = Proxy


-- Property "overwrite-mode"
_overwriteMode :: Proxy "overwrite-mode"
_overwriteMode = Proxy


-- Property "owner-document"
_ownerDocument :: Proxy "owner-document"
_ownerDocument = Proxy


-- Property "owner-element"
_ownerElement :: Proxy "owner-element"
_ownerElement = Proxy


-- Property "owner-node"
_ownerNode :: Proxy "owner-node"
_ownerNode = Proxy


-- Property "owner-rule"
_ownerRule :: Proxy "owner-rule"
_ownerRule = Proxy


-- Property "pack-direction"
_packDirection :: Proxy "pack-direction"
_packDirection = Proxy


-- Property "page"
_page :: Proxy "page"
_page = Proxy


-- Property "page-increment"
_pageIncrement :: Proxy "page-increment"
_pageIncrement = Proxy


-- Property "page-size"
_pageSize :: Proxy "page-size"
_pageSize = Proxy


-- Property "page-x"
_pageX :: Proxy "page-x"
_pageX = Proxy


-- Property "page-x-offset"
_pageXOffset :: Proxy "page-x-offset"
_pageXOffset = Proxy


-- Property "page-y"
_pageY :: Proxy "page-y"
_pageY = Proxy


-- Property "page-y-offset"
_pageYOffset :: Proxy "page-y-offset"
_pageYOffset = Proxy


-- Property "paint-clock"
_paintClock :: Proxy "paint-clock"
_paintClock = Proxy


-- Property "paragraph-background"
_paragraphBackground :: Proxy "paragraph-background"
_paragraphBackground = Proxy


-- Property "paragraph-background-gdk"
_paragraphBackgroundGdk :: Proxy "paragraph-background-gdk"
_paragraphBackgroundGdk = Proxy


-- Property "paragraph-background-rgba"
_paragraphBackgroundRgba :: Proxy "paragraph-background-rgba"
_paragraphBackgroundRgba = Proxy


-- Property "paragraph-background-set"
_paragraphBackgroundSet :: Proxy "paragraph-background-set"
_paragraphBackgroundSet = Proxy


-- Property "parent"
_parent :: Proxy "parent"
_parent = Proxy


-- Property "parent-element"
_parentElement :: Proxy "parent-element"
_parentElement = Proxy


-- Property "parent-node"
_parentNode :: Proxy "parent-node"
_parentNode = Proxy


-- Property "parent-rule"
_parentRule :: Proxy "parent-rule"
_parentRule = Proxy


-- Property "parent-style-sheet"
_parentStyleSheet :: Proxy "parent-style-sheet"
_parentStyleSheet = Proxy


-- Property "paste-target-list"
_pasteTargetList :: Proxy "paste-target-list"
_pasteTargetList = Proxy


-- Property "path"
_path :: Proxy "path"
_path = Proxy


-- Property "pathname"
_pathname :: Proxy "pathname"
_pathname = Proxy


-- Property "pattern"
_pattern :: Proxy "pattern"
_pattern = Proxy


-- Property "pattern-mismatch"
_patternMismatch :: Proxy "pattern-mismatch"
_patternMismatch = Proxy


-- Property "pause-on-exit"
_pauseOnExit :: Proxy "pause-on-exit"
_pauseOnExit = Proxy


-- Property "paused"
_paused :: Proxy "paused"
_paused = Proxy


-- Property "performance"
_performance :: Proxy "performance"
_performance = Proxy


-- Property "permission"
_permission :: Proxy "permission"
_permission = Proxy


-- Property "personalbar"
_personalbar :: Proxy "personalbar"
_personalbar = Proxy


-- Property "ping"
_ping :: Proxy "ping"
_ping = Proxy


-- Property "pixbuf"
_pixbuf :: Proxy "pixbuf"
_pixbuf = Proxy


-- Property "pixbuf-animation"
_pixbufAnimation :: Proxy "pixbuf-animation"
_pixbufAnimation = Proxy


-- Property "pixbuf-column"
_pixbufColumn :: Proxy "pixbuf-column"
_pixbufColumn = Proxy


-- Property "pixbuf-expander-closed"
_pixbufExpanderClosed :: Proxy "pixbuf-expander-closed"
_pixbufExpanderClosed = Proxy


-- Property "pixbuf-expander-open"
_pixbufExpanderOpen :: Proxy "pixbuf-expander-open"
_pixbufExpanderOpen = Proxy


-- Property "pixel-depth"
_pixelDepth :: Proxy "pixel-depth"
_pixelDepth = Proxy


-- Property "pixel-size"
_pixelSize :: Proxy "pixel-size"
_pixelSize = Proxy


-- Property "pixels-above-lines"
_pixelsAboveLines :: Proxy "pixels-above-lines"
_pixelsAboveLines = Proxy


-- Property "pixels-above-lines-set"
_pixelsAboveLinesSet :: Proxy "pixels-above-lines-set"
_pixelsAboveLinesSet = Proxy


-- Property "pixels-below-lines"
_pixelsBelowLines :: Proxy "pixels-below-lines"
_pixelsBelowLines = Proxy


-- Property "pixels-below-lines-set"
_pixelsBelowLinesSet :: Proxy "pixels-below-lines-set"
_pixelsBelowLinesSet = Proxy


-- Property "pixels-inside-wrap"
_pixelsInsideWrap :: Proxy "pixels-inside-wrap"
_pixelsInsideWrap = Proxy


-- Property "pixels-inside-wrap-set"
_pixelsInsideWrapSet :: Proxy "pixels-inside-wrap-set"
_pixelsInsideWrapSet = Proxy


-- Property "placeholder"
_placeholder :: Proxy "placeholder"
_placeholder = Proxy


-- Property "placeholder-text"
_placeholderText :: Proxy "placeholder-text"
_placeholderText = Proxy


-- Property "platform"
_platform :: Proxy "platform"
_platform = Proxy


-- Property "playback-rate"
_playbackRate :: Proxy "playback-rate"
_playbackRate = Proxy


-- Property "playback-state"
_playbackState :: Proxy "playback-state"
_playbackState = Proxy


-- Property "played"
_played :: Proxy "played"
_played = Proxy


-- Property "plugins"
_plugins :: Proxy "plugins"
_plugins = Proxy


-- Property "pointer-before-reference-node"
_pointerBeforeReferenceNode :: Proxy "pointer-before-reference-node"
_pointerBeforeReferenceNode = Proxy


-- Property "pointing-to"
_pointingTo :: Proxy "pointing-to"
_pointingTo = Proxy


-- Property "popover"
_popover :: Proxy "popover"
_popover = Proxy


-- Property "populate-all"
_populateAll :: Proxy "populate-all"
_populateAll = Proxy


-- Property "popup"
_popup :: Proxy "popup"
_popup = Proxy


-- Property "popup-completion"
_popupCompletion :: Proxy "popup-completion"
_popupCompletion = Proxy


-- Property "popup-fixed-width"
_popupFixedWidth :: Proxy "popup-fixed-width"
_popupFixedWidth = Proxy


-- Property "popup-set-width"
_popupSetWidth :: Proxy "popup-set-width"
_popupSetWidth = Proxy


-- Property "popup-shown"
_popupShown :: Proxy "popup-shown"
_popupShown = Proxy


-- Property "popup-single-match"
_popupSingleMatch :: Proxy "popup-single-match"
_popupSingleMatch = Proxy


-- Property "port"
_port :: Proxy "port"
_port = Proxy


-- Property "position"
_position :: Proxy "position"
_position = Proxy


-- Property "position-set"
_positionSet :: Proxy "position-set"
_positionSet = Proxy


-- Property "poster"
_poster :: Proxy "poster"
_poster = Proxy


-- Property "preferred-stylesheet-set"
_preferredStylesheetSet :: Proxy "preferred-stylesheet-set"
_preferredStylesheetSet = Proxy


-- Property "prefix"
_prefix :: Proxy "prefix"
_prefix = Proxy


-- Property "preload"
_preload :: Proxy "preload"
_preload = Proxy


-- Property "preview-text"
_previewText :: Proxy "preview-text"
_previewText = Proxy


-- Property "preview-widget"
_previewWidget :: Proxy "preview-widget"
_previewWidget = Proxy


-- Property "preview-widget-active"
_previewWidgetActive :: Proxy "preview-widget-active"
_previewWidgetActive = Proxy


-- Property "previous-element-sibling"
_previousElementSibling :: Proxy "previous-element-sibling"
_previousElementSibling = Proxy


-- Property "previous-sibling"
_previousSibling :: Proxy "previous-sibling"
_previousSibling = Proxy


-- Property "primary-icon-activatable"
_primaryIconActivatable :: Proxy "primary-icon-activatable"
_primaryIconActivatable = Proxy


-- Property "primary-icon-gicon"
_primaryIconGicon :: Proxy "primary-icon-gicon"
_primaryIconGicon = Proxy


-- Property "primary-icon-name"
_primaryIconName :: Proxy "primary-icon-name"
_primaryIconName = Proxy


-- Property "primary-icon-pixbuf"
_primaryIconPixbuf :: Proxy "primary-icon-pixbuf"
_primaryIconPixbuf = Proxy


-- Property "primary-icon-sensitive"
_primaryIconSensitive :: Proxy "primary-icon-sensitive"
_primaryIconSensitive = Proxy


-- Property "primary-icon-stock"
_primaryIconStock :: Proxy "primary-icon-stock"
_primaryIconStock = Proxy


-- Property "primary-icon-storage-type"
_primaryIconStorageType :: Proxy "primary-icon-storage-type"
_primaryIconStorageType = Proxy


-- Property "primary-icon-tooltip-markup"
_primaryIconTooltipMarkup :: Proxy "primary-icon-tooltip-markup"
_primaryIconTooltipMarkup = Proxy


-- Property "primary-icon-tooltip-text"
_primaryIconTooltipText :: Proxy "primary-icon-tooltip-text"
_primaryIconTooltipText = Proxy


-- Property "print-backgrounds"
_printBackgrounds :: Proxy "print-backgrounds"
_printBackgrounds = Proxy


-- Property "print-settings"
_printSettings :: Proxy "print-settings"
_printSettings = Proxy


-- Property "product"
_product :: Proxy "product"
_product = Proxy


-- Property "product-sub"
_productSub :: Proxy "product-sub"
_productSub = Proxy


-- Property "profile"
_profile :: Proxy "profile"
_profile = Proxy


-- Property "program-name"
_programName :: Proxy "program-name"
_programName = Proxy


-- Property "progress"
_progress :: Proxy "progress"
_progress = Proxy


-- Property "progress-fraction"
_progressFraction :: Proxy "progress-fraction"
_progressFraction = Proxy


-- Property "progress-pulse-step"
_progressPulseStep :: Proxy "progress-pulse-step"
_progressPulseStep = Proxy


-- Property "propagation-phase"
_propagationPhase :: Proxy "propagation-phase"
_propagationPhase = Proxy


-- Property "protocol"
_protocol :: Proxy "protocol"
_protocol = Proxy


-- Property "public-id"
_publicId :: Proxy "public-id"
_publicId = Proxy


-- Property "pulse"
_pulse :: Proxy "pulse"
_pulse = Proxy


-- Property "pulse-step"
_pulseStep :: Proxy "pulse-step"
_pulseStep = Proxy


-- Property "radio"
_radio :: Proxy "radio"
_radio = Proxy


-- Property "range-count"
_rangeCount :: Proxy "range-count"
_rangeCount = Proxy


-- Property "range-overflow"
_rangeOverflow :: Proxy "range-overflow"
_rangeOverflow = Proxy


-- Property "range-underflow"
_rangeUnderflow :: Proxy "range-underflow"
_rangeUnderflow = Proxy


-- Property "ratio"
_ratio :: Proxy "ratio"
_ratio = Proxy


-- Property "read-only"
_readOnly :: Proxy "read-only"
_readOnly = Proxy


-- Property "ready-state"
_readyState :: Proxy "ready-state"
_readyState = Proxy


-- Property "reason"
_reason :: Proxy "reason"
_reason = Proxy


-- Property "receives-default"
_receivesDefault :: Proxy "receives-default"
_receivesDefault = Proxy


-- Property "recent-manager"
_recentManager :: Proxy "recent-manager"
_recentManager = Proxy


-- Property "redirect-count"
_redirectCount :: Proxy "redirect-count"
_redirectCount = Proxy


-- Property "redirect-end"
_redirectEnd :: Proxy "redirect-end"
_redirectEnd = Proxy


-- Property "redirect-start"
_redirectStart :: Proxy "redirect-start"
_redirectStart = Proxy


-- Property "reference-node"
_referenceNode :: Proxy "reference-node"
_referenceNode = Proxy


-- Property "referrer"
_referrer :: Proxy "referrer"
_referrer = Proxy


-- Property "register-session"
_registerSession :: Proxy "register-session"
_registerSession = Proxy


-- Property "rel"
_rel :: Proxy "rel"
_rel = Proxy


-- Property "related-action"
_relatedAction :: Proxy "related-action"
_relatedAction = Proxy


-- Property "related-target"
_relatedTarget :: Proxy "related-target"
_relatedTarget = Proxy


-- Property "relative-to"
_relativeTo :: Proxy "relative-to"
_relativeTo = Proxy


-- Property "relief"
_relief :: Proxy "relief"
_relief = Proxy


-- Property "renderer"
_renderer :: Proxy "renderer"
_renderer = Proxy


-- Property "reorderable"
_reorderable :: Proxy "reorderable"
_reorderable = Proxy


-- Property "report-ur-is"
_reportUrIs :: Proxy "report-ur-is"
_reportUrIs = Proxy


-- Property "request-start"
_requestStart :: Proxy "request-start"
_requestStart = Proxy


-- Property "required"
_required :: Proxy "required"
_required = Proxy


-- Property "reserve-toggle-size"
_reserveToggleSize :: Proxy "reserve-toggle-size"
_reserveToggleSize = Proxy


-- Property "reset-style-inheritance"
_resetStyleInheritance :: Proxy "reset-style-inheritance"
_resetStyleInheritance = Proxy


-- Property "resizable"
_resizable :: Proxy "resizable"
_resizable = Proxy


-- Property "resizable-text-areas"
_resizableTextAreas :: Proxy "resizable-text-areas"
_resizableTextAreas = Proxy


-- Property "resize-grip-visible"
_resizeGripVisible :: Proxy "resize-grip-visible"
_resizeGripVisible = Proxy


-- Property "resize-mode"
_resizeMode :: Proxy "resize-mode"
_resizeMode = Proxy


-- Property "resize-toplevel"
_resizeToplevel :: Proxy "resize-toplevel"
_resizeToplevel = Proxy


-- Property "resource"
_resource :: Proxy "resource"
_resource = Proxy


-- Property "respect-image-orientation"
_respectImageOrientation :: Proxy "respect-image-orientation"
_respectImageOrientation = Proxy


-- Property "response-end"
_responseEnd :: Proxy "response-end"
_responseEnd = Proxy


-- Property "response-start"
_responseStart :: Proxy "response-start"
_responseStart = Proxy


-- Property "restrict-to-fill-level"
_restrictToFillLevel :: Proxy "restrict-to-fill-level"
_restrictToFillLevel = Proxy


-- Property "result-type"
_resultType :: Proxy "result-type"
_resultType = Proxy


-- Property "return-value"
_returnValue :: Proxy "return-value"
_returnValue = Proxy


-- Property "rev"
_rev :: Proxy "rev"
_rev = Proxy


-- Property "reveal-child"
_revealChild :: Proxy "reveal-child"
_revealChild = Proxy


-- Property "reversed"
_reversed :: Proxy "reversed"
_reversed = Proxy


-- Property "rgba"
_rgba :: Proxy "rgba"
_rgba = Proxy


-- Property "right-justified"
_rightJustified :: Proxy "right-justified"
_rightJustified = Proxy


-- Property "right-margin"
_rightMargin :: Proxy "right-margin"
_rightMargin = Proxy


-- Property "right-margin-set"
_rightMarginSet :: Proxy "right-margin-set"
_rightMarginSet = Proxy


-- Property "right-padding"
_rightPadding :: Proxy "right-padding"
_rightPadding = Proxy


-- Property "rise"
_rise :: Proxy "rise"
_rise = Proxy


-- Property "rise-set"
_riseSet :: Proxy "rise-set"
_riseSet = Proxy


-- Property "role"
_role :: Proxy "role"
_role = Proxy


-- Property "root"
_root :: Proxy "root"
_root = Proxy


-- Property "round-digits"
_roundDigits :: Proxy "round-digits"
_roundDigits = Proxy


-- Property "row-homogeneous"
_rowHomogeneous :: Proxy "row-homogeneous"
_rowHomogeneous = Proxy


-- Property "row-index"
_rowIndex :: Proxy "row-index"
_rowIndex = Proxy


-- Property "row-spacing"
_rowSpacing :: Proxy "row-spacing"
_rowSpacing = Proxy


-- Property "row-span"
_rowSpan :: Proxy "row-span"
_rowSpan = Proxy


-- Property "row-span-column"
_rowSpanColumn :: Proxy "row-span-column"
_rowSpanColumn = Proxy


-- Property "rows"
_rows :: Proxy "rows"
_rows = Proxy


-- Property "rubber-banding"
_rubberBanding :: Proxy "rubber-banding"
_rubberBanding = Proxy


-- Property "rules"
_rules :: Proxy "rules"
_rules = Proxy


-- Property "rules-hint"
_rulesHint :: Proxy "rules-hint"
_rulesHint = Proxy


-- Property "sandbox"
_sandbox :: Proxy "sandbox"
_sandbox = Proxy


-- Property "sans-serif-font-family"
_sansSerifFontFamily :: Proxy "sans-serif-font-family"
_sansSerifFontFamily = Proxy


-- Property "scale"
_scale :: Proxy "scale"
_scale = Proxy


-- Property "scale-factor"
_scaleFactor :: Proxy "scale-factor"
_scaleFactor = Proxy


-- Property "scale-set"
_scaleSet :: Proxy "scale-set"
_scaleSet = Proxy


-- Property "scheme"
_scheme :: Proxy "scheme"
_scheme = Proxy


-- Property "scope"
_scope :: Proxy "scope"
_scope = Proxy


-- Property "screen"
_screen :: Proxy "screen"
_screen = Proxy


-- Property "screen-left"
_screenLeft :: Proxy "screen-left"
_screenLeft = Proxy


-- Property "screen-top"
_screenTop :: Proxy "screen-top"
_screenTop = Proxy


-- Property "screen-x"
_screenX :: Proxy "screen-x"
_screenX = Proxy


-- Property "screen-y"
_screenY :: Proxy "screen-y"
_screenY = Proxy


-- Property "scripts"
_scripts :: Proxy "scripts"
_scripts = Proxy


-- Property "scroll-amount"
_scrollAmount :: Proxy "scroll-amount"
_scrollAmount = Proxy


-- Property "scroll-delay"
_scrollDelay :: Proxy "scroll-delay"
_scrollDelay = Proxy


-- Property "scroll-height"
_scrollHeight :: Proxy "scroll-height"
_scrollHeight = Proxy


-- Property "scroll-left"
_scrollLeft :: Proxy "scroll-left"
_scrollLeft = Proxy


-- Property "scroll-offset"
_scrollOffset :: Proxy "scroll-offset"
_scrollOffset = Proxy


-- Property "scroll-top"
_scrollTop :: Proxy "scroll-top"
_scrollTop = Proxy


-- Property "scroll-width"
_scrollWidth :: Proxy "scroll-width"
_scrollWidth = Proxy


-- Property "scroll-x"
_scrollX :: Proxy "scroll-x"
_scrollX = Proxy


-- Property "scroll-y"
_scrollY :: Proxy "scroll-y"
_scrollY = Proxy


-- Property "scrollable"
_scrollable :: Proxy "scrollable"
_scrollable = Proxy


-- Property "scrollbar-visible"
_scrollbarVisible :: Proxy "scrollbar-visible"
_scrollbarVisible = Proxy


-- Property "scrollbars"
_scrollbars :: Proxy "scrollbars"
_scrollbars = Proxy


-- Property "scrolling"
_scrolling :: Proxy "scrolling"
_scrolling = Proxy


-- Property "seamless"
_seamless :: Proxy "seamless"
_seamless = Proxy


-- Property "search"
_search :: Proxy "search"
_search = Proxy


-- Property "search-column"
_searchColumn :: Proxy "search-column"
_searchColumn = Proxy


-- Property "search-mode"
_searchMode :: Proxy "search-mode"
_searchMode = Proxy


-- Property "search-mode-enabled"
_searchModeEnabled :: Proxy "search-mode-enabled"
_searchModeEnabled = Proxy


-- Property "secondary-icon-activatable"
_secondaryIconActivatable :: Proxy "secondary-icon-activatable"
_secondaryIconActivatable = Proxy


-- Property "secondary-icon-gicon"
_secondaryIconGicon :: Proxy "secondary-icon-gicon"
_secondaryIconGicon = Proxy


-- Property "secondary-icon-name"
_secondaryIconName :: Proxy "secondary-icon-name"
_secondaryIconName = Proxy


-- Property "secondary-icon-pixbuf"
_secondaryIconPixbuf :: Proxy "secondary-icon-pixbuf"
_secondaryIconPixbuf = Proxy


-- Property "secondary-icon-sensitive"
_secondaryIconSensitive :: Proxy "secondary-icon-sensitive"
_secondaryIconSensitive = Proxy


-- Property "secondary-icon-stock"
_secondaryIconStock :: Proxy "secondary-icon-stock"
_secondaryIconStock = Proxy


-- Property "secondary-icon-storage-type"
_secondaryIconStorageType :: Proxy "secondary-icon-storage-type"
_secondaryIconStorageType = Proxy


-- Property "secondary-icon-tooltip-markup"
_secondaryIconTooltipMarkup :: Proxy "secondary-icon-tooltip-markup"
_secondaryIconTooltipMarkup = Proxy


-- Property "secondary-icon-tooltip-text"
_secondaryIconTooltipText :: Proxy "secondary-icon-tooltip-text"
_secondaryIconTooltipText = Proxy


-- Property "secondary-text"
_secondaryText :: Proxy "secondary-text"
_secondaryText = Proxy


-- Property "secondary-use-markup"
_secondaryUseMarkup :: Proxy "secondary-use-markup"
_secondaryUseMarkup = Proxy


-- Property "section-row-index"
_sectionRowIndex :: Proxy "section-row-index"
_sectionRowIndex = Proxy


-- Property "secure-connection-start"
_secureConnectionStart :: Proxy "secure-connection-start"
_secureConnectionStart = Proxy


-- Property "security-origin"
_securityOrigin :: Proxy "security-origin"
_securityOrigin = Proxy


-- Property "security-policy"
_securityPolicy :: Proxy "security-policy"
_securityPolicy = Proxy


-- Property "seekable"
_seekable :: Proxy "seekable"
_seekable = Proxy


-- Property "seeking"
_seeking :: Proxy "seeking"
_seeking = Proxy


-- Property "select-multiple"
_selectMultiple :: Proxy "select-multiple"
_selectMultiple = Proxy


-- Property "selectable"
_selectable :: Proxy "selectable"
_selectable = Proxy


-- Property "selected"
_selected :: Proxy "selected"
_selected = Proxy


-- Property "selected-files"
_selectedFiles :: Proxy "selected-files"
_selectedFiles = Proxy


-- Property "selected-index"
_selectedIndex :: Proxy "selected-index"
_selectedIndex = Proxy


-- Property "selected-options"
_selectedOptions :: Proxy "selected-options"
_selectedOptions = Proxy


-- Property "selected-stylesheet-set"
_selectedStylesheetSet :: Proxy "selected-stylesheet-set"
_selectedStylesheetSet = Proxy


-- Property "selection-bound"
_selectionBound :: Proxy "selection-bound"
_selectionBound = Proxy


-- Property "selection-direction"
_selectionDirection :: Proxy "selection-direction"
_selectionDirection = Proxy


-- Property "selection-end"
_selectionEnd :: Proxy "selection-end"
_selectionEnd = Proxy


-- Property "selection-mode"
_selectionMode :: Proxy "selection-mode"
_selectionMode = Proxy


-- Property "selection-start"
_selectionStart :: Proxy "selection-start"
_selectionStart = Proxy


-- Property "self"
_self :: Proxy "self"
_self = Proxy


-- Property "self-scrolling"
_selfScrolling :: Proxy "self-scrolling"
_selfScrolling = Proxy


-- Property "sensitive"
_sensitive :: Proxy "sensitive"
_sensitive = Proxy


-- Property "serif-font-family"
_serifFontFamily :: Proxy "serif-font-family"
_serifFontFamily = Proxy


-- Property "session-storage"
_sessionStorage :: Proxy "session-storage"
_sessionStorage = Proxy


-- Property "settings"
_settings :: Proxy "settings"
_settings = Proxy


-- Property "shadow-type"
_shadowType :: Proxy "shadow-type"
_shadowType = Proxy


-- Property "shape"
_shape :: Proxy "shape"
_shape = Proxy


-- Property "sheet"
_sheet :: Proxy "sheet"
_sheet = Proxy


-- Property "shift-key"
_shiftKey :: Proxy "shift-key"
_shiftKey = Proxy


-- Property "short-label"
_shortLabel :: Proxy "short-label"
_shortLabel = Proxy


-- Property "show-all"
_showAll :: Proxy "show-all"
_showAll = Proxy


-- Property "show-arrow"
_showArrow :: Proxy "show-arrow"
_showArrow = Proxy


-- Property "show-border"
_showBorder :: Proxy "show-border"
_showBorder = Proxy


-- Property "show-close-button"
_showCloseButton :: Proxy "show-close-button"
_showCloseButton = Proxy


-- Property "show-connect-to-server"
_showConnectToServer :: Proxy "show-connect-to-server"
_showConnectToServer = Proxy


-- Property "show-day-names"
_showDayNames :: Proxy "show-day-names"
_showDayNames = Proxy


-- Property "show-default"
_showDefault :: Proxy "show-default"
_showDefault = Proxy


-- Property "show-default-item"
_showDefaultItem :: Proxy "show-default-item"
_showDefaultItem = Proxy


-- Property "show-desktop"
_showDesktop :: Proxy "show-desktop"
_showDesktop = Proxy


-- Property "show-details"
_showDetails :: Proxy "show-details"
_showDetails = Proxy


-- Property "show-dialog-item"
_showDialogItem :: Proxy "show-dialog-item"
_showDialogItem = Proxy


-- Property "show-editor"
_showEditor :: Proxy "show-editor"
_showEditor = Proxy


-- Property "show-enter-location"
_showEnterLocation :: Proxy "show-enter-location"
_showEnterLocation = Proxy


-- Property "show-expanders"
_showExpanders :: Proxy "show-expanders"
_showExpanders = Proxy


-- Property "show-fallback"
_showFallback :: Proxy "show-fallback"
_showFallback = Proxy


-- Property "show-fill-level"
_showFillLevel :: Proxy "show-fill-level"
_showFillLevel = Proxy


-- Property "show-heading"
_showHeading :: Proxy "show-heading"
_showHeading = Proxy


-- Property "show-hidden"
_showHidden :: Proxy "show-hidden"
_showHidden = Proxy


-- Property "show-icons"
_showIcons :: Proxy "show-icons"
_showIcons = Proxy


-- Property "show-menubar"
_showMenubar :: Proxy "show-menubar"
_showMenubar = Proxy


-- Property "show-not-found"
_showNotFound :: Proxy "show-not-found"
_showNotFound = Proxy


-- Property "show-numbers"
_showNumbers :: Proxy "show-numbers"
_showNumbers = Proxy


-- Property "show-other"
_showOther :: Proxy "show-other"
_showOther = Proxy


-- Property "show-preview-entry"
_showPreviewEntry :: Proxy "show-preview-entry"
_showPreviewEntry = Proxy


-- Property "show-private"
_showPrivate :: Proxy "show-private"
_showPrivate = Proxy


-- Property "show-progress"
_showProgress :: Proxy "show-progress"
_showProgress = Proxy


-- Property "show-recommended"
_showRecommended :: Proxy "show-recommended"
_showRecommended = Proxy


-- Property "show-size"
_showSize :: Proxy "show-size"
_showSize = Proxy


-- Property "show-style"
_showStyle :: Proxy "show-style"
_showStyle = Proxy


-- Property "show-tabs"
_showTabs :: Proxy "show-tabs"
_showTabs = Proxy


-- Property "show-text"
_showText :: Proxy "show-text"
_showText = Proxy


-- Property "show-tips"
_showTips :: Proxy "show-tips"
_showTips = Proxy


-- Property "show-week-numbers"
_showWeekNumbers :: Proxy "show-week-numbers"
_showWeekNumbers = Proxy


-- Property "single-line-mode"
_singleLineMode :: Proxy "single-line-mode"
_singleLineMode = Proxy


-- Property "single-node-value"
_singleNodeValue :: Proxy "single-node-value"
_singleNodeValue = Proxy


-- Property "single-paragraph-mode"
_singleParagraphMode :: Proxy "single-paragraph-mode"
_singleParagraphMode = Proxy


-- Property "size"
_size :: Proxy "size"
_size = Proxy


-- Property "size-points"
_sizePoints :: Proxy "size-points"
_sizePoints = Proxy


-- Property "size-set"
_sizeSet :: Proxy "size-set"
_sizeSet = Proxy


-- Property "sizing"
_sizing :: Proxy "sizing"
_sizing = Proxy


-- Property "skip-pager-hint"
_skipPagerHint :: Proxy "skip-pager-hint"
_skipPagerHint = Proxy


-- Property "skip-taskbar-hint"
_skipTaskbarHint :: Proxy "skip-taskbar-hint"
_skipTaskbarHint = Proxy


-- Property "snap-edge"
_snapEdge :: Proxy "snap-edge"
_snapEdge = Proxy


-- Property "snap-edge-set"
_snapEdgeSet :: Proxy "snap-edge-set"
_snapEdgeSet = Proxy


-- Property "snap-to-lines"
_snapToLines :: Proxy "snap-to-lines"
_snapToLines = Proxy


-- Property "snap-to-ticks"
_snapToTicks :: Proxy "snap-to-ticks"
_snapToTicks = Proxy


-- Property "snapshot-length"
_snapshotLength :: Proxy "snapshot-length"
_snapshotLength = Proxy


-- Property "socket-window"
_socketWindow :: Proxy "socket-window"
_socketWindow = Proxy


-- Property "sort-column-id"
_sortColumnId :: Proxy "sort-column-id"
_sortColumnId = Proxy


-- Property "sort-indicator"
_sortIndicator :: Proxy "sort-indicator"
_sortIndicator = Proxy


-- Property "sort-order"
_sortOrder :: Proxy "sort-order"
_sortOrder = Proxy


-- Property "sort-type"
_sortType :: Proxy "sort-type"
_sortType = Proxy


-- Property "spacing"
_spacing :: Proxy "spacing"
_spacing = Proxy


-- Property "span"
_span :: Proxy "span"
_span = Proxy


-- Property "specified"
_specified :: Proxy "specified"
_specified = Proxy


-- Property "spell-checking-languages"
_spellCheckingLanguages :: Proxy "spell-checking-languages"
_spellCheckingLanguages = Proxy


-- Property "spellcheck"
_spellcheck :: Proxy "spellcheck"
_spellcheck = Proxy


-- Property "src"
_src :: Proxy "src"
_src = Proxy


-- Property "src-element"
_srcElement :: Proxy "src-element"
_srcElement = Proxy


-- Property "srcdoc"
_srcdoc :: Proxy "srcdoc"
_srcdoc = Proxy


-- Property "srcset"
_srcset :: Proxy "srcset"
_srcset = Proxy


-- Property "stack"
_stack :: Proxy "stack"
_stack = Proxy


-- Property "standby"
_standby :: Proxy "standby"
_standby = Proxy


-- Property "start"
_start :: Proxy "start"
_start = Proxy


-- Property "start-container"
_startContainer :: Proxy "start-container"
_startContainer = Proxy


-- Property "start-offset"
_startOffset :: Proxy "start-offset"
_startOffset = Proxy


-- Property "start-time"
_startTime :: Proxy "start-time"
_startTime = Proxy


-- Property "startup-id"
_startupId :: Proxy "startup-id"
_startupId = Proxy


-- Property "state"
_state :: Proxy "state"
_state = Proxy


-- Property "status"
_status :: Proxy "status"
_status = Proxy


-- Property "status-string"
_statusString :: Proxy "status-string"
_statusString = Proxy


-- Property "statusbar"
_statusbar :: Proxy "statusbar"
_statusbar = Proxy


-- Property "statusbar-visible"
_statusbarVisible :: Proxy "statusbar-visible"
_statusbarVisible = Proxy


-- Property "step"
_step :: Proxy "step"
_step = Proxy


-- Property "step-increment"
_stepIncrement :: Proxy "step-increment"
_stepIncrement = Proxy


-- Property "step-mismatch"
_stepMismatch :: Proxy "step-mismatch"
_stepMismatch = Proxy


-- Property "stock"
_stock :: Proxy "stock"
_stock = Proxy


-- Property "stock-detail"
_stockDetail :: Proxy "stock-detail"
_stockDetail = Proxy


-- Property "stock-id"
_stockId :: Proxy "stock-id"
_stockId = Proxy


-- Property "stock-size"
_stockSize :: Proxy "stock-size"
_stockSize = Proxy


-- Property "storage-type"
_storageType :: Proxy "storage-type"
_storageType = Proxy


-- Property "stretch"
_stretch :: Proxy "stretch"
_stretch = Proxy


-- Property "stretch-set"
_stretchSet :: Proxy "stretch-set"
_stretchSet = Proxy


-- Property "strikethrough"
_strikethrough :: Proxy "strikethrough"
_strikethrough = Proxy


-- Property "strikethrough-rgba"
_strikethroughRgba :: Proxy "strikethrough-rgba"
_strikethroughRgba = Proxy


-- Property "strikethrough-rgba-set"
_strikethroughRgbaSet :: Proxy "strikethrough-rgba-set"
_strikethroughRgbaSet = Proxy


-- Property "strikethrough-set"
_strikethroughSet :: Proxy "strikethrough-set"
_strikethroughSet = Proxy


-- Property "string-value"
_stringValue :: Proxy "string-value"
_stringValue = Proxy


-- Property "style"
_style :: Proxy "style"
_style = Proxy


-- Property "style-context"
_styleContext :: Proxy "style-context"
_styleContext = Proxy


-- Property "style-media"
_styleMedia :: Proxy "style-media"
_styleMedia = Proxy


-- Property "style-set"
_styleSet :: Proxy "style-set"
_styleSet = Proxy


-- Property "style-sheets"
_styleSheets :: Proxy "style-sheets"
_styleSheets = Proxy


-- Property "submenu"
_submenu :: Proxy "submenu"
_submenu = Proxy


-- Property "subtitle"
_subtitle :: Proxy "subtitle"
_subtitle = Proxy


-- Property "suffixes"
_suffixes :: Proxy "suffixes"
_suffixes = Proxy


-- Property "suggested-filename"
_suggestedFilename :: Proxy "suggested-filename"
_suggestedFilename = Proxy


-- Property "summary"
_summary :: Proxy "summary"
_summary = Proxy


-- Property "support-selection"
_supportSelection :: Proxy "support-selection"
_supportSelection = Proxy


-- Property "surface"
_surface :: Proxy "surface"
_surface = Proxy


-- Property "system-id"
_systemId :: Proxy "system-id"
_systemId = Proxy


-- Property "t-bodies"
_tBodies :: Proxy "t-bodies"
_tBodies = Proxy


-- Property "t-foot"
_tFoot :: Proxy "t-foot"
_tFoot = Proxy


-- Property "t-head"
_tHead :: Proxy "t-head"
_tHead = Proxy


-- Property "tab-index"
_tabIndex :: Proxy "tab-index"
_tabIndex = Proxy


-- Property "tab-key-cycles-through-elements"
_tabKeyCyclesThroughElements :: Proxy "tab-key-cycles-through-elements"
_tabKeyCyclesThroughElements = Proxy


-- Property "tab-pos"
_tabPos :: Proxy "tab-pos"
_tabPos = Proxy


-- Property "tabs"
_tabs :: Proxy "tabs"
_tabs = Proxy


-- Property "tabs-set"
_tabsSet :: Proxy "tabs-set"
_tabsSet = Proxy


-- Property "tag-name"
_tagName :: Proxy "tag-name"
_tagName = Proxy


-- Property "tag-table"
_tagTable :: Proxy "tag-table"
_tagTable = Proxy


-- Property "take-focus"
_takeFocus :: Proxy "take-focus"
_takeFocus = Proxy


-- Property "target"
_target :: Proxy "target"
_target = Proxy


-- Property "target-frame"
_targetFrame :: Proxy "target-frame"
_targetFrame = Proxy


-- Property "tearoff-state"
_tearoffState :: Proxy "tearoff-state"
_tearoffState = Proxy


-- Property "tearoff-title"
_tearoffTitle :: Proxy "tearoff-title"
_tearoffTitle = Proxy


-- Property "text"
_text :: Proxy "text"
_text = Proxy


-- Property "text-column"
_textColumn :: Proxy "text-column"
_textColumn = Proxy


-- Property "text-content"
_textContent :: Proxy "text-content"
_textContent = Proxy


-- Property "text-length"
_textLength :: Proxy "text-length"
_textLength = Proxy


-- Property "text-lock"
_textLock :: Proxy "text-lock"
_textLock = Proxy


-- Property "text-tracks"
_textTracks :: Proxy "text-tracks"
_textTracks = Proxy


-- Property "text-unlock"
_textUnlock :: Proxy "text-unlock"
_textUnlock = Proxy


-- Property "text-xalign"
_textXalign :: Proxy "text-xalign"
_textXalign = Proxy


-- Property "text-yalign"
_textYalign :: Proxy "text-yalign"
_textYalign = Proxy


-- Property "time-stamp"
_timeStamp :: Proxy "time-stamp"
_timeStamp = Proxy


-- Property "timeline-profiling-enabled"
_timelineProfilingEnabled :: Proxy "timeline-profiling-enabled"
_timelineProfilingEnabled = Proxy


-- Property "timestamp"
_timestamp :: Proxy "timestamp"
_timestamp = Proxy


-- Property "timing"
_timing :: Proxy "timing"
_timing = Proxy


-- Property "title"
_title :: Proxy "title"
_title = Proxy


-- Property "to-element"
_toElement :: Proxy "to-element"
_toElement = Proxy


-- Property "too-long"
_tooLong :: Proxy "too-long"
_tooLong = Proxy


-- Property "toolbar"
_toolbar :: Proxy "toolbar"
_toolbar = Proxy


-- Property "toolbar-style"
_toolbarStyle :: Proxy "toolbar-style"
_toolbarStyle = Proxy


-- Property "toolbar-visible"
_toolbarVisible :: Proxy "toolbar-visible"
_toolbarVisible = Proxy


-- Property "tooltip"
_tooltip :: Proxy "tooltip"
_tooltip = Proxy


-- Property "tooltip-column"
_tooltipColumn :: Proxy "tooltip-column"
_tooltipColumn = Proxy


-- Property "tooltip-lock"
_tooltipLock :: Proxy "tooltip-lock"
_tooltipLock = Proxy


-- Property "tooltip-markup"
_tooltipMarkup :: Proxy "tooltip-markup"
_tooltipMarkup = Proxy


-- Property "tooltip-not-authorized"
_tooltipNotAuthorized :: Proxy "tooltip-not-authorized"
_tooltipNotAuthorized = Proxy


-- Property "tooltip-text"
_tooltipText :: Proxy "tooltip-text"
_tooltipText = Proxy


-- Property "tooltip-unlock"
_tooltipUnlock :: Proxy "tooltip-unlock"
_tooltipUnlock = Proxy


-- Property "top"
_top :: Proxy "top"
_top = Proxy


-- Property "top-padding"
_topPadding :: Proxy "top-padding"
_topPadding = Proxy


-- Property "total-frame-delay"
_totalFrameDelay :: Proxy "total-frame-delay"
_totalFrameDelay = Proxy


-- Property "total-js-heap-size"
_totalJsHeapSize :: Proxy "total-js-heap-size"
_totalJsHeapSize = Proxy


-- Property "total-size"
_totalSize :: Proxy "total-size"
_totalSize = Proxy


-- Property "total-video-frames"
_totalVideoFrames :: Proxy "total-video-frames"
_totalVideoFrames = Proxy


-- Property "touch-only"
_touchOnly :: Proxy "touch-only"
_touchOnly = Proxy


-- Property "track"
_track :: Proxy "track"
_track = Proxy


-- Property "track-print-status"
_trackPrintStatus :: Proxy "track-print-status"
_trackPrintStatus = Proxy


-- Property "track-visited-links"
_trackVisitedLinks :: Proxy "track-visited-links"
_trackVisitedLinks = Proxy


-- Property "transient-for"
_transientFor :: Proxy "transient-for"
_transientFor = Proxy


-- Property "transition-duration"
_transitionDuration :: Proxy "transition-duration"
_transitionDuration = Proxy


-- Property "transition-running"
_transitionRunning :: Proxy "transition-running"
_transitionRunning = Proxy


-- Property "transition-type"
_transitionType :: Proxy "transition-type"
_transitionType = Proxy


-- Property "transitions-enabled"
_transitionsEnabled :: Proxy "transitions-enabled"
_transitionsEnabled = Proxy


-- Property "translate"
_translate :: Proxy "translate"
_translate = Proxy


-- Property "translation-domain"
_translationDomain :: Proxy "translation-domain"
_translationDomain = Proxy


-- Property "translator-credits"
_translatorCredits :: Proxy "translator-credits"
_translatorCredits = Proxy


-- Property "transparent"
_transparent :: Proxy "transparent"
_transparent = Proxy


-- Property "true-speed"
_trueSpeed :: Proxy "true-speed"
_trueSpeed = Proxy


-- Property "truncate-multiline"
_truncateMultiline :: Proxy "truncate-multiline"
_truncateMultiline = Proxy


-- Property "type"
_type :: Proxy "type"
_type = Proxy


-- Property "type-hint"
_typeHint :: Proxy "type-hint"
_typeHint = Proxy


-- Property "type-mismatch"
_typeMismatch :: Proxy "type-mismatch"
_typeMismatch = Proxy


-- Property "ui"
_ui :: Proxy "ui"
_ui = Proxy


-- Property "underline"
_underline :: Proxy "underline"
_underline = Proxy


-- Property "underline-rgba"
_underlineRgba :: Proxy "underline-rgba"
_underlineRgba = Proxy


-- Property "underline-rgba-set"
_underlineRgbaSet :: Proxy "underline-rgba-set"
_underlineRgbaSet = Proxy


-- Property "underline-set"
_underlineSet :: Proxy "underline-set"
_underlineSet = Proxy


-- Property "unit"
_unit :: Proxy "unit"
_unit = Proxy


-- Property "unload-event-end"
_unloadEventEnd :: Proxy "unload-event-end"
_unloadEventEnd = Proxy


-- Property "unload-event-start"
_unloadEventStart :: Proxy "unload-event-start"
_unloadEventStart = Proxy


-- Property "update-policy"
_updatePolicy :: Proxy "update-policy"
_updatePolicy = Proxy


-- Property "upper"
_upper :: Proxy "upper"
_upper = Proxy


-- Property "upper-stepper-sensitivity"
_upperStepperSensitivity :: Proxy "upper-stepper-sensitivity"
_upperStepperSensitivity = Proxy


-- Property "urgency-hint"
_urgencyHint :: Proxy "urgency-hint"
_urgencyHint = Proxy


-- Property "uri"
_uri :: Proxy "uri"
_uri = Proxy


-- Property "url"
_url :: Proxy "url"
_url = Proxy


-- Property "use-action-appearance"
_useActionAppearance :: Proxy "use-action-appearance"
_useActionAppearance = Proxy


-- Property "use-alpha"
_useAlpha :: Proxy "use-alpha"
_useAlpha = Proxy


-- Property "use-fallback"
_useFallback :: Proxy "use-fallback"
_useFallback = Proxy


-- Property "use-font"
_useFont :: Proxy "use-font"
_useFont = Proxy


-- Property "use-full-page"
_useFullPage :: Proxy "use-full-page"
_useFullPage = Proxy


-- Property "use-header-bar"
_useHeaderBar :: Proxy "use-header-bar"
_useHeaderBar = Proxy


-- Property "use-map"
_useMap :: Proxy "use-map"
_useMap = Proxy


-- Property "use-markup"
_useMarkup :: Proxy "use-markup"
_useMarkup = Proxy


-- Property "use-popover"
_usePopover :: Proxy "use-popover"
_usePopover = Proxy


-- Property "use-preview-label"
_usePreviewLabel :: Proxy "use-preview-label"
_usePreviewLabel = Proxy


-- Property "use-size"
_useSize :: Proxy "use-size"
_useSize = Proxy


-- Property "use-stock"
_useStock :: Proxy "use-stock"
_useStock = Proxy


-- Property "use-symbolic"
_useSymbolic :: Proxy "use-symbolic"
_useSymbolic = Proxy


-- Property "use-underline"
_useUnderline :: Proxy "use-underline"
_useUnderline = Proxy


-- Property "used-js-heap-size"
_usedJsHeapSize :: Proxy "used-js-heap-size"
_usedJsHeapSize = Proxy


-- Property "user-agent"
_userAgent :: Proxy "user-agent"
_userAgent = Proxy


-- Property "user-scalable"
_userScalable :: Proxy "user-scalable"
_userScalable = Proxy


-- Property "user-stylesheet-uri"
_userStylesheetUri :: Proxy "user-stylesheet-uri"
_userStylesheetUri = Proxy


-- Property "v-align"
_vAlign :: Proxy "v-align"
_vAlign = Proxy


-- Property "v-link"
_vLink :: Proxy "v-link"
_vLink = Proxy


-- Property "vadjustment"
_vadjustment :: Proxy "vadjustment"
_vadjustment = Proxy


-- Property "valid"
_valid :: Proxy "valid"
_valid = Proxy


-- Property "validation-message"
_validationMessage :: Proxy "validation-message"
_validationMessage = Proxy


-- Property "validity"
_validity :: Proxy "validity"
_validity = Proxy


-- Property "valign"
_valign :: Proxy "valign"
_valign = Proxy


-- Property "value"
_value :: Proxy "value"
_value = Proxy


-- Property "value-as-number"
_valueAsNumber :: Proxy "value-as-number"
_valueAsNumber = Proxy


-- Property "value-missing"
_valueMissing :: Proxy "value-missing"
_valueMissing = Proxy


-- Property "value-pos"
_valuePos :: Proxy "value-pos"
_valuePos = Proxy


-- Property "value-type"
_valueType :: Proxy "value-type"
_valueType = Proxy


-- Property "variant"
_variant :: Proxy "variant"
_variant = Proxy


-- Property "variant-set"
_variantSet :: Proxy "variant-set"
_variantSet = Proxy


-- Property "vendor"
_vendor :: Proxy "vendor"
_vendor = Proxy


-- Property "vendor-sub"
_vendorSub :: Proxy "vendor-sub"
_vendorSub = Proxy


-- Property "version"
_version :: Proxy "version"
_version = Proxy


-- Property "vertical"
_vertical :: Proxy "vertical"
_vertical = Proxy


-- Property "vertical-scrollbar-policy"
_verticalScrollbarPolicy :: Proxy "vertical-scrollbar-policy"
_verticalScrollbarPolicy = Proxy


-- Property "vexpand"
_vexpand :: Proxy "vexpand"
_vexpand = Proxy


-- Property "vexpand-set"
_vexpandSet :: Proxy "vexpand-set"
_vexpandSet = Proxy


-- Property "vhomogeneous"
_vhomogeneous :: Proxy "vhomogeneous"
_vhomogeneous = Proxy


-- Property "video-height"
_videoHeight :: Proxy "video-height"
_videoHeight = Proxy


-- Property "video-tracks"
_videoTracks :: Proxy "video-tracks"
_videoTracks = Proxy


-- Property "video-width"
_videoWidth :: Proxy "video-width"
_videoWidth = Proxy


-- Property "view"
_view :: Proxy "view"
_view = Proxy


-- Property "view-mode"
_viewMode :: Proxy "view-mode"
_viewMode = Proxy


-- Property "viewport-attributes"
_viewportAttributes :: Proxy "viewport-attributes"
_viewportAttributes = Proxy


-- Property "virtual-root"
_virtualRoot :: Proxy "virtual-root"
_virtualRoot = Proxy


-- Property "visibility"
_visibility :: Proxy "visibility"
_visibility = Proxy


-- Property "visibility-state"
_visibilityState :: Proxy "visibility-state"
_visibilityState = Proxy


-- Property "visible"
_visible :: Proxy "visible"
_visible = Proxy


-- Property "visible-child"
_visibleChild :: Proxy "visible-child"
_visibleChild = Proxy


-- Property "visible-child-name"
_visibleChildName :: Proxy "visible-child-name"
_visibleChildName = Proxy


-- Property "visible-horizontal"
_visibleHorizontal :: Proxy "visible-horizontal"
_visibleHorizontal = Proxy


-- Property "visible-overflown"
_visibleOverflown :: Proxy "visible-overflown"
_visibleOverflown = Proxy


-- Property "visible-submenu"
_visibleSubmenu :: Proxy "visible-submenu"
_visibleSubmenu = Proxy


-- Property "visible-vertical"
_visibleVertical :: Proxy "visible-vertical"
_visibleVertical = Proxy


-- Property "visible-window"
_visibleWindow :: Proxy "visible-window"
_visibleWindow = Proxy


-- Property "visited"
_visited :: Proxy "visited"
_visited = Proxy


-- Property "vlink-color"
_vlinkColor :: Proxy "vlink-color"
_vlinkColor = Proxy


-- Property "volume"
_volume :: Proxy "volume"
_volume = Proxy


-- Property "vscroll-policy"
_vscrollPolicy :: Proxy "vscroll-policy"
_vscrollPolicy = Proxy


-- Property "vscrollbar-policy"
_vscrollbarPolicy :: Proxy "vscrollbar-policy"
_vscrollbarPolicy = Proxy


-- Property "vspace"
_vspace :: Proxy "vspace"
_vspace = Proxy


-- Property "web-database-quota"
_webDatabaseQuota :: Proxy "web-database-quota"
_webDatabaseQuota = Proxy


-- Property "web-database-usage"
_webDatabaseUsage :: Proxy "web-database-usage"
_webDatabaseUsage = Proxy


-- Property "web-inspector"
_webInspector :: Proxy "web-inspector"
_webInspector = Proxy


-- Property "web-view"
_webView :: Proxy "web-view"
_webView = Proxy


-- Property "webkit-audio-decoded-byte-count"
_webkitAudioDecodedByteCount :: Proxy "webkit-audio-decoded-byte-count"
_webkitAudioDecodedByteCount = Proxy


-- Property "webkit-battery"
_webkitBattery :: Proxy "webkit-battery"
_webkitBattery = Proxy


-- Property "webkit-closed-captions-visible"
_webkitClosedCaptionsVisible :: Proxy "webkit-closed-captions-visible"
_webkitClosedCaptionsVisible = Proxy


-- Property "webkit-current-full-screen-element"
_webkitCurrentFullScreenElement :: Proxy "webkit-current-full-screen-element"
_webkitCurrentFullScreenElement = Proxy


-- Property "webkit-current-playback-target-is-wireless"
_webkitCurrentPlaybackTargetIsWireless :: Proxy "webkit-current-playback-target-is-wireless"
_webkitCurrentPlaybackTargetIsWireless = Proxy


-- Property "webkit-decoded-frame-count"
_webkitDecodedFrameCount :: Proxy "webkit-decoded-frame-count"
_webkitDecodedFrameCount = Proxy


-- Property "webkit-direction-inverted-from-device"
_webkitDirectionInvertedFromDevice :: Proxy "webkit-direction-inverted-from-device"
_webkitDirectionInvertedFromDevice = Proxy


-- Property "webkit-displaying-fullscreen"
_webkitDisplayingFullscreen :: Proxy "webkit-displaying-fullscreen"
_webkitDisplayingFullscreen = Proxy


-- Property "webkit-dropped-frame-count"
_webkitDroppedFrameCount :: Proxy "webkit-dropped-frame-count"
_webkitDroppedFrameCount = Proxy


-- Property "webkit-force"
_webkitForce :: Proxy "webkit-force"
_webkitForce = Proxy


-- Property "webkit-full-screen-keyboard-input-allowed"
_webkitFullScreenKeyboardInputAllowed :: Proxy "webkit-full-screen-keyboard-input-allowed"
_webkitFullScreenKeyboardInputAllowed = Proxy


-- Property "webkit-fullscreen-element"
_webkitFullscreenElement :: Proxy "webkit-fullscreen-element"
_webkitFullscreenElement = Proxy


-- Property "webkit-fullscreen-enabled"
_webkitFullscreenEnabled :: Proxy "webkit-fullscreen-enabled"
_webkitFullscreenEnabled = Proxy


-- Property "webkit-grammar"
_webkitGrammar :: Proxy "webkit-grammar"
_webkitGrammar = Proxy


-- Property "webkit-has-closed-captions"
_webkitHasClosedCaptions :: Proxy "webkit-has-closed-captions"
_webkitHasClosedCaptions = Proxy


-- Property "webkit-is-full-screen"
_webkitIsFullScreen :: Proxy "webkit-is-full-screen"
_webkitIsFullScreen = Proxy


-- Property "webkit-movement-x"
_webkitMovementX :: Proxy "webkit-movement-x"
_webkitMovementX = Proxy


-- Property "webkit-movement-y"
_webkitMovementY :: Proxy "webkit-movement-y"
_webkitMovementY = Proxy


-- Property "webkit-persistent-storage"
_webkitPersistentStorage :: Proxy "webkit-persistent-storage"
_webkitPersistentStorage = Proxy


-- Property "webkit-pointer-lock-element"
_webkitPointerLockElement :: Proxy "webkit-pointer-lock-element"
_webkitPointerLockElement = Proxy


-- Property "webkit-preserves-pitch"
_webkitPreservesPitch :: Proxy "webkit-preserves-pitch"
_webkitPreservesPitch = Proxy


-- Property "webkit-radius-x"
_webkitRadiusX :: Proxy "webkit-radius-x"
_webkitRadiusX = Proxy


-- Property "webkit-radius-y"
_webkitRadiusY :: Proxy "webkit-radius-y"
_webkitRadiusY = Proxy


-- Property "webkit-region-overset"
_webkitRegionOverset :: Proxy "webkit-region-overset"
_webkitRegionOverset = Proxy


-- Property "webkit-relative-path"
_webkitRelativePath :: Proxy "webkit-relative-path"
_webkitRelativePath = Proxy


-- Property "webkit-rotation-angle"
_webkitRotationAngle :: Proxy "webkit-rotation-angle"
_webkitRotationAngle = Proxy


-- Property "webkit-speech"
_webkitSpeech :: Proxy "webkit-speech"
_webkitSpeech = Proxy


-- Property "webkit-storage-info"
_webkitStorageInfo :: Proxy "webkit-storage-info"
_webkitStorageInfo = Proxy


-- Property "webkit-supports-fullscreen"
_webkitSupportsFullscreen :: Proxy "webkit-supports-fullscreen"
_webkitSupportsFullscreen = Proxy


-- Property "webkit-temporary-storage"
_webkitTemporaryStorage :: Proxy "webkit-temporary-storage"
_webkitTemporaryStorage = Proxy


-- Property "webkit-video-decoded-byte-count"
_webkitVideoDecodedByteCount :: Proxy "webkit-video-decoded-byte-count"
_webkitVideoDecodedByteCount = Proxy


-- Property "webkit-wireless-video-playback-disabled"
_webkitWirelessVideoPlaybackDisabled :: Proxy "webkit-wireless-video-playback-disabled"
_webkitWirelessVideoPlaybackDisabled = Proxy


-- Property "webkitdirectory"
_webkitdirectory :: Proxy "webkitdirectory"
_webkitdirectory = Proxy


-- Property "webkitdropzone"
_webkitdropzone :: Proxy "webkitdropzone"
_webkitdropzone = Proxy


-- Property "website"
_website :: Proxy "website"
_website = Proxy


-- Property "website-label"
_websiteLabel :: Proxy "website-label"
_websiteLabel = Proxy


-- Property "weight"
_weight :: Proxy "weight"
_weight = Proxy


-- Property "weight-set"
_weightSet :: Proxy "weight-set"
_weightSet = Proxy


-- Property "what-to-show"
_whatToShow :: Proxy "what-to-show"
_whatToShow = Proxy


-- Property "wheel-delta"
_wheelDelta :: Proxy "wheel-delta"
_wheelDelta = Proxy


-- Property "wheel-delta-x"
_wheelDeltaX :: Proxy "wheel-delta-x"
_wheelDeltaX = Proxy


-- Property "wheel-delta-y"
_wheelDeltaY :: Proxy "wheel-delta-y"
_wheelDeltaY = Proxy


-- Property "which"
_which :: Proxy "which"
_which = Proxy


-- Property "whole-text"
_wholeText :: Proxy "whole-text"
_wholeText = Proxy


-- Property "wide-handle"
_wideHandle :: Proxy "wide-handle"
_wideHandle = Proxy


-- Property "widget"
_widget :: Proxy "widget"
_widget = Proxy


-- Property "width"
_width :: Proxy "width"
_width = Proxy


-- Property "width-chars"
_widthChars :: Proxy "width-chars"
_widthChars = Proxy


-- Property "width-request"
_widthRequest :: Proxy "width-request"
_widthRequest = Proxy


-- Property "will-validate"
_willValidate :: Proxy "will-validate"
_willValidate = Proxy


-- Property "window"
_window :: Proxy "window"
_window = Proxy


-- Property "window-features"
_windowFeatures :: Proxy "window-features"
_windowFeatures = Proxy


-- Property "window-placement"
_windowPlacement :: Proxy "window-placement"
_windowPlacement = Proxy


-- Property "window-placement-set"
_windowPlacementSet :: Proxy "window-placement-set"
_windowPlacementSet = Proxy


-- Property "window-position"
_windowPosition :: Proxy "window-position"
_windowPosition = Proxy


-- Property "wrap"
_wrap :: Proxy "wrap"
_wrap = Proxy


-- Property "wrap-license"
_wrapLicense :: Proxy "wrap-license"
_wrapLicense = Proxy


-- Property "wrap-mode"
_wrapMode :: Proxy "wrap-mode"
_wrapMode = Proxy


-- Property "wrap-mode-set"
_wrapModeSet :: Proxy "wrap-mode-set"
_wrapModeSet = Proxy


-- Property "wrap-width"
_wrapWidth :: Proxy "wrap-width"
_wrapWidth = Proxy


-- Property "x"
_x :: Proxy "x"
_x = Proxy


-- Property "x-offset"
_xOffset :: Proxy "x-offset"
_xOffset = Proxy


-- Property "xalign"
_xalign :: Proxy "xalign"
_xalign = Proxy


-- Property "xml-encoding"
_xmlEncoding :: Proxy "xml-encoding"
_xmlEncoding = Proxy


-- Property "xml-standalone"
_xmlStandalone :: Proxy "xml-standalone"
_xmlStandalone = Proxy


-- Property "xml-version"
_xmlVersion :: Proxy "xml-version"
_xmlVersion = Proxy


-- Property "xpad"
_xpad :: Proxy "xpad"
_xpad = Proxy


-- Property "xscale"
_xscale :: Proxy "xscale"
_xscale = Proxy


-- Property "y"
_y :: Proxy "y"
_y = Proxy


-- Property "yalign"
_yalign :: Proxy "yalign"
_yalign = Proxy


-- Property "year"
_year :: Proxy "year"
_year = Proxy


-- Property "ypad"
_ypad :: Proxy "ypad"
_ypad = Proxy


-- Property "yscale"
_yscale :: Proxy "yscale"
_yscale = Proxy


-- Property "zoom-level"
_zoomLevel :: Proxy "zoom-level"
_zoomLevel = Proxy


-- Property "zoom-step"
_zoomStep :: Proxy "zoom-step"
_zoomStep = Proxy



