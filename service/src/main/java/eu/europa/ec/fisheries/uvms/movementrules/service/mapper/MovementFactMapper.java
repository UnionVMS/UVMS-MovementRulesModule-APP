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
package eu.europa.ec.fisheries.uvms.movementrules.service.mapper;

import java.util.List;
import java.util.UUID;

import eu.europa.ec.fisheries.schema.mobileterminal.types.v1.ComChannelAttribute;
import eu.europa.ec.fisheries.schema.mobileterminal.types.v1.ComChannelType;
import eu.europa.ec.fisheries.schema.mobileterminal.types.v1.MobileTerminalAttribute;
import eu.europa.ec.fisheries.schema.mobileterminal.types.v1.MobileTerminalType;
import eu.europa.ec.fisheries.schema.movement.v1.MovementMetaDataAreaType;
import eu.europa.ec.fisheries.schema.movement.v1.MovementType;
import eu.europa.ec.fisheries.schema.movement.v1.MovementTypeType;
import eu.europa.ec.fisheries.uvms.asset.client.model.AssetMTEnrichmentResponse;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.MovementReportProcessorBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.MovementFact;
import eu.europa.ec.fisheries.wsdl.asset.group.AssetGroup;
import eu.europa.ec.fisheries.wsdl.asset.types.Asset;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MovementFactMapper {

    private static final Logger LOG = LoggerFactory.getLogger(MovementFactMapper.class);


    private MovementFactMapper() {
    }

    public static MovementFact mapMovementFact(MovementType movement, AssetMTEnrichmentResponse response, String comChannelType, Long timeDiffInSeconds, Integer numberOfReportsLast24Hours, String channelGuid, List<String> vicinityOf) {
        if (movement == null) {
            throw new IllegalArgumentException("Movement was null, asset: " + response.getAssetUUID() );
        }
        MovementFact fact = new MovementFact();

        fact.setChannelGuid(channelGuid);

        fact.setMovementMovement(movement);
        fact.setMovementGuid(movement.getGuid());

        // ROOT
        if (response.getAssetGroupList() != null) {
            for (String assetGroup : response.getAssetGroupList()) {
                fact.getAssetGroups().add(assetGroup.toString());
            }
        }

        // ACTIVITY
        if (movement.getActivity() != null) {
            fact.setActivityCallback(movement.getActivity().getCallback());
            fact.setActivityMessageId(movement.getActivity().getMessageId());
            if (movement.getActivity().getMessageType() != null) {
                fact.setActivityMessageType(movement.getActivity().getMessageType().name());
            }
        }

        // AREA
        if (movement.getMetaData() != null) {
            List<MovementMetaDataAreaType> areas = movement.getMetaData().getAreas();
            for (MovementMetaDataAreaType area : areas) {
                if (MovementTypeType.POS.equals(area.getTransitionType()) || MovementTypeType.ENT.equals(area.getTransitionType())) {
                    fact.getAreaCodes().add(area.getCode());
                    fact.getAreaTypes().add(area.getAreaType());
                }
                if (MovementTypeType.ENT.equals(area.getTransitionType())) {
                    fact.getEntAreaCodes().add(area.getName());
                    fact.getAreaCodes().add(area.getCode());
                    fact.getEntAreaTypes().add(area.getAreaType());
                }
                if (MovementTypeType.EXI.equals(area.getTransitionType())) {
                    fact.getExtAreaCodes().add(area.getName());
                    fact.getAreaCodes().add(area.getCode());
                    fact.getExtAreaTypes().add(area.getAreaType());
                }
            }
        }

        // ASSET
        String gearType = response.getGearType();
        String externalMarking = response.getExternalMarking();
        String countryCode = response.getFlagstate();
        String cfr = response.getCfr();
        String ircs = response.getIrcs();
        String assetName = response.getAssetName();
        UUID assetId = response.getAssetUUID() == null ? null : UUID.fromString(response.getAssetUUID());
        String assetStatus = response.getAssetStatus();
        String mmsiNo = response.getMmsi();

        // ASSET
        if (response.getAssetUUID() != null && !response.getAssetUUID().isEmpty()) {
            fact.setAssetIdGearType(gearType);
            fact.setExternalMarking(externalMarking);
            fact.setFlagState(countryCode);
            fact.setCfr(cfr);
            fact.setIrcs(ircs);
            fact.setAssetName(assetName);
            fact.setAssetGuid(assetId.toString());
            fact.setAssetStatus(assetStatus);
            fact.setMmsiNo(mmsiNo);
        }

        String mobileTerminalGuid = response.getMobileTerminalGuid();
        String mobileTerminalType = response.getMobileTerminalType();
        String dnid = response.getDNID();
        String memberNumber = response.getMemberNumber();
        String serialNumber = response.getSerialNumber();
        Boolean  mobileTerminalIsInactive = response.getMobileTerminalIsInactive();
        // MOBILE_TERMINAL
        if (response.getMobileTerminalGuid() != null && !response.getMobileTerminalGuid().isEmpty()) {
            fact.setMobileTerminalGuid(mobileTerminalGuid);
            fact.setComChannelType(comChannelType);
            fact.setMobileTerminalType(mobileTerminalType);
            fact.setMobileTerminalDnid(dnid);
            fact.setMobileTerminalMemberNumber(memberNumber);
            fact.setMobileTerminalSerialNumber(serialNumber);
            fact.setMobileTerminalStatus(mobileTerminalIsInactive  ? "INACTIVE" : "ACTIVE");
        }

        // POSITION
        if (movement.getPosition() != null) {
            fact.setAltitude(movement.getPosition().getAltitude());
            fact.setLatitude(movement.getPosition().getLatitude());
            fact.setLongitude(movement.getPosition().getLongitude());
        }
        fact.setCalculatedCourse(movement.getCalculatedCourse());
        fact.setCalculatedSpeed(movement.getCalculatedSpeed());
        if (movement.getMovementType() != null) {
            fact.setMovementType(movement.getMovementType().name());
        }
        if (movement.getPositionTime() != null) {
            fact.setPositionTime(movement.getPositionTime());
        }
        fact.setReportedCourse(movement.getReportedCourse());
        fact.setReportedSpeed(movement.getReportedSpeed());
        if (movement.getMetaData() != null) {
            if (movement.getMetaData().getFromSegmentType() != null) {
                fact.setSegmentType(movement.getMetaData().getFromSegmentType().name());
            }
            if (movement.getMetaData().getClosestCountry() != null) {
                fact.setClosestCountryCode(movement.getMetaData().getClosestCountry().getCode());
            }
            if (movement.getMetaData().getClosestPort() != null) {
                fact.setClosestPortCode(movement.getMetaData().getClosestPort().getCode());
            }
        }
        if (movement.getSource() != null) {
            fact.setSource(movement.getSource().name());
        }
        fact.setStatusCode(movement.getStatus());
        // TODO
        fact.setVicinityOf(vicinityOf);

        // REPORT
        fact.setTimeDiffPositionReport(timeDiffInSeconds);
        fact.setSumPositionReport(numberOfReportsLast24Hours);

        return fact;
    }

}