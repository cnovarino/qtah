-- This file is part of Qtah.
--
-- Copyright 2015-2016 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Graphics.UI.Qtah.Internal.Interface.Widgets.QGroupBox (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Type (TBitspace, TObj, TPtr),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkCtor,
  mkProp,
  mkProps,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (bs_Alignment)
import Graphics.UI.Qtah.Internal.Interface.Listener (
  c_ListenerBool,
  )
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QGroupBox"] $
  (QtExport $ ExportClass c_QGroupBox) :
  map QtExportSignal signals

c_QGroupBox =
  addReqIncludes [includeStd "QGroupBox"] $
  makeClass (ident "QGroupBox") Nothing [c_QWidget]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  , mkCtor "newWithTitle" [TObj c_QString]
  , mkCtor "newWithTitleAndParent" [TObj c_QString, TPtr $ TObj c_QWidget]
  ] $
  mkProps
  [ mkProp "alignment" $ TBitspace bs_Alignment
  , mkBoolIsProp "checkable"
  , mkBoolIsProp "checked"
  , mkBoolIsProp "flat"
  , mkProp "title" $ TObj c_QString
  ]

signals =
  collect
  [ test (qtVersion >= [4, 2]) $ makeSignal c_QGroupBox "clicked" c_ListenerBool
  , just $ makeSignal c_QGroupBox "toggled" c_ListenerBool
  ]
