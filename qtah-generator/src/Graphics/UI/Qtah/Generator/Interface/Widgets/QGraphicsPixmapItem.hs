-- This file is part of Qtah.
--
-- Copyright 2015-2018 The Qtah Authors.
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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsPixmapItem (
  aModule,
  c_QGraphicsPixmapItem,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkMethod,
  mkMethod',
  mkConstMethod,
  mkCtor,
  )
import Foreign.Hoppy.Generator.Types (voidT, objT)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qreal)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPixmap (c_QPixmap)
import Graphics.UI.Qtah.Generator.Interface.Core.QPointF (c_QPointF)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsItem
  (c_QGraphicsItem)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QGraphicsPixmapItem"]
  [ QtExport $ ExportClass c_QGraphicsPixmapItem
  ]

c_QGraphicsPixmapItem =
  addReqIncludes [includeStd "QGraphicsPixmapItem"] $
  classSetEntityPrefix "" $
  makeClass (ident "QGraphicsPixmapItem") Nothing [c_QGraphicsItem]
  [ mkCtor "new" []
  , mkConstMethod "offset" [] $ objT c_QPointF
  , mkConstMethod "pixmap" [] $ objT c_QPixmap
  , mkMethod' "setOffset" "setOffset" [qreal, qreal] voidT
  , mkMethod "setPixmap" [objT c_QPixmap] voidT
  ]
