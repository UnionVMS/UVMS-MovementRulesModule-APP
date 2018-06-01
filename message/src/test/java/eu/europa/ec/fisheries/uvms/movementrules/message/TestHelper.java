/*
﻿Developed with the contribution of the European Commission - Directorate General for Maritime Affairs and Fisheries
© European Union, 2015-2016.

This file is part of the Integrated Fisheries Data Management (IFDM) Suite. The IFDM Suite is free software: you can
redistribute it and/or modify it under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or any later version. The IFDM Suite is distributed in
the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a
copy of the GNU General Public License along with the IFDM Suite. If not, see <http://www.gnu.org/licenses/>.
 */
package eu.europa.ec.fisheries.uvms.movementrules.message;

import java.util.Date;
import eu.europa.ec.fisheries.schema.movementrules.asset.v1.AssetId;
import eu.europa.ec.fisheries.schema.movementrules.asset.v1.AssetIdList;
import eu.europa.ec.fisheries.schema.movementrules.asset.v1.AssetIdType;
import eu.europa.ec.fisheries.schema.movementrules.asset.v1.AssetType;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.MovementComChannelType;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.MovementPoint;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.MovementSourceType;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.MovementTypeType;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.RawMovementType;

public class TestHelper {

    public static RawMovementType createBasicMovement() {
        RawMovementType movement = new RawMovementType();
        AssetId assetId = new AssetId();
        assetId.setAssetType(AssetType.VESSEL);
        AssetIdList assetIdList = new AssetIdList();
        assetIdList.setIdType(AssetIdType.IRCS);
        assetIdList.setValue("IRCS1234");
        assetId.getAssetIdList().add(assetIdList);
        movement.setAssetId(assetId);
        movement.setAssetName("Test Asset");
        movement.setFlagState("SWE");
        movement.setDateRecieved(new Date());
        movement.setMovementType(MovementTypeType.POS);
        movement.setPluginName("PLUGIN");
        movement.setPluginType("NAF");
        MovementPoint movementPoint = new MovementPoint();
        movementPoint.setLatitude(56d);
        movementPoint.setLongitude(11d);
        movement.setPosition(movementPoint);
        movement.setPositionTime(new Date());
        movement.setReportedCourse(50d);
        movement.setReportedSpeed(5d);
        movement.setSource(MovementSourceType.INMARSAT_C);
        movement.setComChannelType(MovementComChannelType.NAF);
        return movement;
    }
}
