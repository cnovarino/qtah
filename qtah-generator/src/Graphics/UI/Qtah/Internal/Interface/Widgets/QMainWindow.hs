{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Widgets.QMainWindow (
  qtModule,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_ListenerQSize)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QMenu (c_QMenu)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QMenuBar (c_QMenuBar)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)
#include "../Mk.hs.inc"

qtModule = makeQtModuleForClass c_QMainWindow $ map QtExportSignal signals

this = c_QMainWindow

c_QMainWindow =
  addReqIncludes [includeStd "QMainWindow"] $
  makeClass (ident "QMainWindow") Nothing [c_QWidget]
  [ _mkCtor "new" []
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
    -- TODO Ctor with Qt::WindowFlags.
  ] $
  [ -- TODO addDockWidget
    -- TODO addToolBar
    -- TODO addToolBarBreak
    -- TODO corner
    _mkMethod "createPopupMenu" [] $ TPtr $ TObj c_QMenu
    -- TODO dockWidgetArea
    -- TODO insertToolBar
    -- TODO insertToolBarBreak
    -- TODO removeDockWidget
    -- TODO restoreState
    -- TODO saveState
    -- TODO setCorner
    -- TODO setTabPosition
    -- TODO setTabShape
    -- TODO splitDockWidget
    -- TODO tabifiedDockWidgets
    -- TODO tabifyDockWidget
    -- TODO tabPosition
    -- TODO tabShape
    -- TODO toolBarArea
    -- TODO toolBarBreak
  ] ++
  _props
  [ _mkBoolIsProp "animated"
  , _mkProp "centralWidget" $ TPtr $ TObj c_QWidget
  , _mkBoolIsProp "dockNestingEnabled"
    -- TODO dockOptions
  , _mkProp "documentMode" TBool
  , _mkProp "iconSize" $ TObj c_QSize
  , _mkProp "menuBar" $ TPtr $ TObj c_QMenuBar
  , _mkProp "menuWidget" $ TPtr $ TObj c_QWidget
    -- TODO statusBar
    -- TODO tabShape
    -- TODO toolButtonStyle
  , _mkProp "unifiedTitleAndToolBarOnMac" TBool
  ]

signals =
  [ _mkSignal "iconSizeChanged" c_ListenerQSize
    -- TODO toolButtonStyleChanged
  ]