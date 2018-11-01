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
package eu.europa.ec.fisheries.uvms.movementrules.model.dto;

import java.util.Date;
import java.util.List;
import javax.json.bind.annotation.JsonbDateFormat;

public class MovementDetails {

    /*
     * FROM MOVEMENT
     */
    private String movementGuid;
    // ACTIVITY
    private String activityCallback;
    private String activityMessageId;
    private String activityMessageType;
    // ASSET
    private String assetGuid;
    private String assetIdGearType;
    private String externalMarking;
    private String flagState;
    private String cfr;
    private String ircs;
    private String assetName;
    private String assetStatus;
    private String mmsi;
    private List<String> assetGroups;

    // MOBILE_TERMINAL
    private String channelGuid;
    private String mobileTerminalGuid;
    private String comChannelType;
    private String mobileTerminalType;
    private String mobileTerminalDnid;
    private String mobileTerminalMemberNumber;
    private String mobileTerminalSerialNumber;
    private String mobileTerminalStatus;

    // POSITION
    private Double altitude;
    private Double latitude;
    private Double longitude;
    private Double calculatedCourse;
    private Double calculatedSpeed;
    private String movementType; // MovementTypeType
    // TODO use instant
    @JsonbDateFormat(value = "yyyy-MM-dd'T'HH:mm:ss.SSSX")
    private Date positionTime;
    private Double reportedCourse;
    private Double reportedSpeed;
    private String segmentType;
    private String source;
    private String statusCode;

    private Double previousLatitude;
    private Double previousLongitude;

    // REPORT
    private List<String> vicinityOf;
    private Integer sumPositionReport;
    
    /*
     * COLLECTED DATA
     */
    private Long timeDiffPositionReport;
    
    private List<String> areaCodes;
    private List<String> areaTypes;
    private List<String> entAreaCodes;
    private List<String> entAreaTypes;
    private List<String> extAreaCodes;
    private List<String> extAreaTypes;

    private String closestCountryCode;
    private String closestPortCode;

    /*
     * Additional attributes used when sending to plugin
     * TODO check if these are needed
     */
    private Double closestPortDistance;
    private Double closestCountryDistance;
    private String connectId;
    private Double tripNumber;
    private String wkt;
    private String internalReferenceNumber;
    private String assetType;
    
    public String getMovementGuid() {
        return movementGuid;
    }

    public void setMovementGuid(String movementGuid) {
        this.movementGuid = movementGuid;
    }

    public String getActivityCallback() {
        return activityCallback;
    }

    public void setActivityCallback(String activityCallback) {
        this.activityCallback = activityCallback;
    }

    public String getActivityMessageId() {
        return activityMessageId;
    }

    public void setActivityMessageId(String activityMessageId) {
        this.activityMessageId = activityMessageId;
    }

    public String getActivityMessageType() {
        return activityMessageType;
    }

    public void setActivityMessageType(String activityMessageType) {
        this.activityMessageType = activityMessageType;
    }

    public String getAssetGuid() {
        return assetGuid;
    }

    public void setAssetGuid(String assetGuid) {
        this.assetGuid = assetGuid;
    }

    public String getAssetIdGearType() {
        return assetIdGearType;
    }

    public void setAssetIdGearType(String assetIdGearType) {
        this.assetIdGearType = assetIdGearType;
    }

    public String getExternalMarking() {
        return externalMarking;
    }

    public void setExternalMarking(String externalMarking) {
        this.externalMarking = externalMarking;
    }

    public String getFlagState() {
        return flagState;
    }

    public void setFlagState(String flagState) {
        this.flagState = flagState;
    }

    public String getCfr() {
        return cfr;
    }

    public void setCfr(String cfr) {
        this.cfr = cfr;
    }

    public String getIrcs() {
        return ircs;
    }

    public void setIrcs(String ircs) {
        this.ircs = ircs;
    }

    public String getAssetName() {
        return assetName;
    }

    public void setAssetName(String assetName) {
        this.assetName = assetName;
    }

    public String getAssetStatus() {
        return assetStatus;
    }

    public void setAssetStatus(String assetStatus) {
        this.assetStatus = assetStatus;
    }

    public String getMmsi() {
        return mmsi;
    }

    public void setMmsi(String mmsi) {
        this.mmsi = mmsi;
    }

    public List<String> getAssetGroups() {
        return assetGroups;
    }

    public void setAssetGroups(List<String> assetGroups) {
        this.assetGroups = assetGroups;
    }

    public String getChannelGuid() {
        return channelGuid;
    }

    public void setChannelGuid(String channelGuid) {
        this.channelGuid = channelGuid;
    }

    public String getMobileTerminalGuid() {
        return mobileTerminalGuid;
    }

    public void setMobileTerminalGuid(String mobileTerminalGuid) {
        this.mobileTerminalGuid = mobileTerminalGuid;
    }

    public String getComChannelType() {
        return comChannelType;
    }

    public void setComChannelType(String comChannelType) {
        this.comChannelType = comChannelType;
    }

    public String getMobileTerminalType() {
        return mobileTerminalType;
    }

    public void setMobileTerminalType(String mobileTerminalType) {
        this.mobileTerminalType = mobileTerminalType;
    }

    public String getMobileTerminalDnid() {
        return mobileTerminalDnid;
    }

    public void setMobileTerminalDnid(String mobileTerminalDnid) {
        this.mobileTerminalDnid = mobileTerminalDnid;
    }

    public String getMobileTerminalMemberNumber() {
        return mobileTerminalMemberNumber;
    }

    public void setMobileTerminalMemberNumber(String mobileTerminalMemberNumber) {
        this.mobileTerminalMemberNumber = mobileTerminalMemberNumber;
    }

    public String getMobileTerminalSerialNumber() {
        return mobileTerminalSerialNumber;
    }

    public void setMobileTerminalSerialNumber(String mobileTerminalSerialNumber) {
        this.mobileTerminalSerialNumber = mobileTerminalSerialNumber;
    }

    public String getMobileTerminalStatus() {
        return mobileTerminalStatus;
    }

    public void setMobileTerminalStatus(String mobileTerminalStatus) {
        this.mobileTerminalStatus = mobileTerminalStatus;
    }

    public Double getAltitude() {
        return altitude;
    }

    public void setAltitude(Double altitude) {
        this.altitude = altitude;
    }

    public Double getLatitude() {
        return latitude;
    }

    public void setLatitude(Double latitude) {
        this.latitude = latitude;
    }

    public Double getLongitude() {
        return longitude;
    }

    public void setLongitude(Double longitude) {
        this.longitude = longitude;
    }

    public Double getCalculatedCourse() {
        return calculatedCourse;
    }

    public void setCalculatedCourse(Double calculatedCourse) {
        this.calculatedCourse = calculatedCourse;
    }

    public Double getCalculatedSpeed() {
        return calculatedSpeed;
    }

    public void setCalculatedSpeed(Double calculatedSpeed) {
        this.calculatedSpeed = calculatedSpeed;
    }

    public String getMovementType() {
        return movementType;
    }

    public void setMovementType(String movementType) {
        this.movementType = movementType;
    }

    public Date getPositionTime() {
        return positionTime;
    }

    public void setPositionTime(Date positionTime) {
        this.positionTime = positionTime;
    }

    public Double getReportedCourse() {
        return reportedCourse;
    }

    public void setReportedCourse(Double reportedCourse) {
        this.reportedCourse = reportedCourse;
    }

    public Double getReportedSpeed() {
        return reportedSpeed;
    }

    public void setReportedSpeed(Double reportedSpeed) {
        this.reportedSpeed = reportedSpeed;
    }

    public String getSegmentType() {
        return segmentType;
    }

    public void setSegmentType(String segmentType) {
        this.segmentType = segmentType;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public String getStatusCode() {
        return statusCode;
    }

    public void setStatusCode(String statusCode) {
        this.statusCode = statusCode;
    }

    public Double getPreviousLatitude() {
        return previousLatitude;
    }

    public void setPreviousLatitude(Double previousLatitude) {
        this.previousLatitude = previousLatitude;
    }

    public Double getPreviousLongitude() {
        return previousLongitude;
    }

    public void setPreviousLongitude(Double previousLongitude) {
        this.previousLongitude = previousLongitude;
    }

    public List<String> getVicinityOf() {
        return vicinityOf;
    }

    public void setVicinityOf(List<String> vicinityOf) {
        this.vicinityOf = vicinityOf;
    }

    public Long getTimeDiffPositionReport() {
        return timeDiffPositionReport;
    }

    public void setTimeDiffPositionReport(Long timeDiffPositionReport) {
        this.timeDiffPositionReport = timeDiffPositionReport;
    }

    public Integer getSumPositionReport() {
        return sumPositionReport;
    }

    public void setSumPositionReport(Integer sumPositionReport) {
        this.sumPositionReport = sumPositionReport;
    }

    public List<String> getAreaCodes() {
        return areaCodes;
    }

    public void setAreaCodes(List<String> areaCodes) {
        this.areaCodes = areaCodes;
    }

    public List<String> getAreaTypes() {
        return areaTypes;
    }

    public void setAreaTypes(List<String> areaTypes) {
        this.areaTypes = areaTypes;
    }

    public List<String> getEntAreaCodes() {
        return entAreaCodes;
    }

    public void setEntAreaCodes(List<String> entAreaCodes) {
        this.entAreaCodes = entAreaCodes;
    }

    public List<String> getEntAreaTypes() {
        return entAreaTypes;
    }

    public void setEntAreaTypes(List<String> entAreaTypes) {
        this.entAreaTypes = entAreaTypes;
    }

    public List<String> getExtAreaCodes() {
        return extAreaCodes;
    }

    public void setExtAreaCodes(List<String> extAreaCodes) {
        this.extAreaCodes = extAreaCodes;
    }

    public List<String> getExtAreaTypes() {
        return extAreaTypes;
    }

    public void setExtAreaTypes(List<String> extAreaTypes) {
        this.extAreaTypes = extAreaTypes;
    }

    public String getClosestCountryCode() {
        return closestCountryCode;
    }

    public void setClosestCountryCode(String closestCountryCode) {
        this.closestCountryCode = closestCountryCode;
    }

    public String getClosestPortCode() {
        return closestPortCode;
    }

    public void setClosestPortCode(String closestPortCode) {
        this.closestPortCode = closestPortCode;
    }

    public Double getClosestPortDistance() {
        return closestPortDistance;
    }

    public void setClosestPortDistance(Double closestPortDistance) {
        this.closestPortDistance = closestPortDistance;
    }

    public Double getClosestCountryDistance() {
        return closestCountryDistance;
    }

    public void setClosestCountryDistance(Double closestCountryDistance) {
        this.closestCountryDistance = closestCountryDistance;
    }

    public String getConnectId() {
        return connectId;
    }

    public void setConnectId(String connectId) {
        this.connectId = connectId;
    }

    public Double getTripNumber() {
        return tripNumber;
    }

    public void setTripNumber(Double tripNumber) {
        this.tripNumber = tripNumber;
    }

    public String getWkt() {
        return wkt;
    }

    public void setWkt(String wkt) {
        this.wkt = wkt;
    }

    public String getInternalReferenceNumber() {
        return internalReferenceNumber;
    }

    public void setInternalReferenceNumber(String internalReferenceNumber) {
        this.internalReferenceNumber = internalReferenceNumber;
    }

    public String getAssetType() {
        return assetType;
    }

    public void setAssetType(String assetType) {
        this.assetType = assetType;
    }
}
