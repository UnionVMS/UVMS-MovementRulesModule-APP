/*
 *
 * Developed by the European Commission - Directorate General for Maritime Affairs and Fisheries © European Union, 2015-2016.
 *
 * This file is part of the Integrated Fisheries Data Management (IFDM) Suite. The IFDM Suite is free software: you can redistribute it
 * and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of
 * the License, or any later version. The IFDM Suite is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with the IFDM Suite. If not, see <http://www.gnu.org/licenses/>.
 *
 *
 */

package eu.europa.ec.fisheries.uvms.rules.service.mapper.fact;

import eu.europa.ec.fisheries.uvms.rules.service.business.fact.CodeType;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.FaArrivalFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.FaCatchFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.FaDeclarationOfArrivalFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.FaDepartureFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.FaDiscardFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.FaEntryToSeaFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.FaExitFromSeaFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.FaFishingOperationFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.FaJointFishingOperationFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.FaLandingFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.FaNotificationOfArrivalFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.FaNotificationOfTranshipmentFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.FaQueryFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.FaQueryParameterFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.FaRelocationFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.FaReportDocumentFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.FaResponseFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.FaTranshipmentFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.FishingActivityFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.FishingGearFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.FishingTripFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.FluxCharacteristicsFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.FluxFaReportMessageFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.FluxLocationFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.GearCharacteristicsFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.GearProblemFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.IdType;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.MeasureType;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.StructuredAddressFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.VesselStorageCharacteristicsFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.fact.VesselTransportMeansFact;
import eu.europa.ec.fisheries.uvms.rules.service.mapper.CustomMapper;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;
import un.unece.uncefact.data.standard.fluxfareportmessage._3.FLUXFAReportMessage;
import un.unece.uncefact.data.standard.fluxresponsemessage._6.FLUXResponseMessage;
import un.unece.uncefact.data.standard.reusableaggregatebusinessinformationentity._20.FACatch;
import un.unece.uncefact.data.standard.reusableaggregatebusinessinformationentity._20.FAQuery;
import un.unece.uncefact.data.standard.reusableaggregatebusinessinformationentity._20.FAQueryParameter;
import un.unece.uncefact.data.standard.reusableaggregatebusinessinformationentity._20.FAReportDocument;
import un.unece.uncefact.data.standard.reusableaggregatebusinessinformationentity._20.FLUXCharacteristic;
import un.unece.uncefact.data.standard.reusableaggregatebusinessinformationentity._20.FLUXLocation;
import un.unece.uncefact.data.standard.reusableaggregatebusinessinformationentity._20.FishingActivity;
import un.unece.uncefact.data.standard.reusableaggregatebusinessinformationentity._20.FishingGear;
import un.unece.uncefact.data.standard.reusableaggregatebusinessinformationentity._20.FishingTrip;
import un.unece.uncefact.data.standard.reusableaggregatebusinessinformationentity._20.GearCharacteristic;
import un.unece.uncefact.data.standard.reusableaggregatebusinessinformationentity._20.GearProblem;
import un.unece.uncefact.data.standard.reusableaggregatebusinessinformationentity._20.StructuredAddress;
import un.unece.uncefact.data.standard.reusableaggregatebusinessinformationentity._20.VesselStorageCharacteristic;
import un.unece.uncefact.data.standard.reusableaggregatebusinessinformationentity._20.VesselTransportMeans;

import java.util.List;

/**
 * @author padhyad
 * @author Gregory Rinaldi
 */
@Mapper(uses = CustomMapper.class)
public interface ActivityFactMapper {

    ActivityFactMapper INSTANCE = Mappers.getMapper(ActivityFactMapper.class);
    String AAP_PRODUCT_PACKAGING_UNIT_QUANTITY = "PackagingUnitQuantity";
    String AAP_PRODUCT_WEIGHT_MEASURE = "WeightMeasure";
    String AAP_PRODUCT_AVERAGE_WEIGHT_MEASURE = "AverageWeightMeasure";
    String AAP_PRODUCT_UNIT_QUANTITY = "UnitQuantity";
    String CODE_TYPE_FOR_FACATCH_FLUXLOCATION = "facatchFluxlocationTypeCode";
    String CODE_TYPE_FOR_FACATCH = "facatchTypeCode";

    @Mappings({
            @Mapping(target = "acceptanceDateTime", source = "acceptanceDateTime"),
            @Mapping(target = "creationDateTime", source = "relatedFLUXReportDocument.creationDateTime"),
            @Mapping(target = "purposeCode", source = "relatedFLUXReportDocument.purposeCode"),
            @Mapping(target = "ids", source = "relatedFLUXReportDocument.IDS"),
            @Mapping(target = "ownerFluxPartyIds", source = "relatedFLUXReportDocument.ownerFLUXParty.IDS"),
            @Mapping(target = "uniqueIds", source = "relatedFLUXReportDocument")
    })
    FaReportDocumentFact generateFactForFaReportDocument(FAReportDocument faReportDocuments);

    List<FaReportDocumentFact> generateFactForFaReportDocuments(List<FAReportDocument> faReportDocuments);

    @Mappings({
            @Mapping(target = "delimitedPeriods", source = "specifiedDelimitedPeriods"),
            @Mapping(target = "operationQuantity", source = "operationsQuantity.value")
    })
    FishingActivityFact generateFactForFishingActivity(FishingActivity fishingActivity);

    List<FishingActivityFact> generateFactForFishingActivities(List<FishingActivity> fishingActivities);

    @Mappings({
            @Mapping(target = "referencedID", source = "FLUXReportDocument.referencedID.value"),
            @Mapping(target = "creationDateTime", source = "FLUXReportDocument.creationDateTime"),
            @Mapping(target = "purposeCode", source = "FLUXReportDocument.purposeCode"),
            @Mapping(target = "ids", source = "FLUXReportDocument.IDS"),
            @Mapping(target = "ownerFluxPartyIds", source = "FLUXReportDocument.ownerFLUXParty.IDS"),
            @Mapping(target = "faReportDocuments", source = "FAReportDocuments"),
            @Mapping(target = "uniqueIds", source = "FLUXReportDocument")
    })
    FluxFaReportMessageFact generateFactForFluxReportMessage(FLUXFAReportMessage fluxfaReportMessage);

    @Mappings({
            @Mapping(target = "ids", source = "vesselTransportMean.IDS"),
            @Mapping(target = "registrationVesselCountryId", source = "vesselTransportMean.registrationVesselCountry.ID"),
            @Mapping(target = "specifiedContactPartyRoleCodes", source = "vesselTransportMean.specifiedContactParties"),
            @Mapping(target = "specifiedContactPersons", source = "vesselTransportMean.specifiedContactParties"),
    })
    VesselTransportMeansFact generateFactForVesselTransportMean(VesselTransportMeans vesselTransportMean);

    List<VesselTransportMeansFact> generateFactForVesselTransportMeans(List<VesselTransportMeans> vesselTransportMean);

    @Mappings({
            @Mapping(target = "postcodeCode", source = "postcodeCode.value"),
            @Mapping(target = "streetName", source = "streetName.value"),
            @Mapping(target = "cityName", source = "cityName.value"),
            @Mapping(target = "countryID", source = "countryID.value"),
            @Mapping(target = "plotIdentification", source = "plotIdentification.value")
    })
    StructuredAddressFact generateFactsForStructureAddress(StructuredAddress structuredAddress);

    List<StructuredAddressFact> generateFactsForStructureAddresses(List<StructuredAddress> structuredAddresses);

    @Mappings({
            @Mapping(target = "typeCode", source = "typeCode")
    })
    FishingGearFact generateFactsForFishingGear(FishingGear fishingGear);

    List<FishingGearFact> generateFactsForFishingGears(List<FishingGear> fishingGears);

    @Mappings({
    })
    GearCharacteristicsFact generateFactsForGearCharacteristic(GearCharacteristic gearCharacteristic);

    List<GearCharacteristicsFact> generateFactsForGearCharacteristics(List<GearCharacteristic> gearCharacteristics);

    @Mappings({
            @Mapping(target = "typeCode", source = "typeCode.value")
    })
    GearProblemFact generateFactsForGearProblem(GearProblem gearProblem);

    List<GearProblemFact> generateFactsForGearProblems(List<GearProblem> gearProblems);


    @Mappings({
            @Mapping(target = "typeCode", source = "faCatches.typeCode"),
            @Mapping(target = "speciesCode", source = "faCatches.speciesCode"),
            @Mapping(target = "sizeDistributionClassCode", source = "faCatches.specifiedSizeDistribution.classCodes"),
            @Mapping(target = "resultAAPProduct", source = "faCatches.appliedAAPProcesses"),
            @Mapping(target = "appliedAAPProcessTypeCodes", expression = "java(CustomMapper.getAppliedProcessTypeCodes(faCatches.getAppliedAAPProcesses()))"),
            @Mapping(target = "resultAAPProductPackagingTypeCode", expression = "java(CustomMapper.getAAPProductPackagingTypeCode(faCatches.getAppliedAAPProcesses()))"),
            @Mapping(target = "resultAAPProductPackagingUnitQuantity", expression = "java(CustomMapper.getMeasureTypeFromAAPProcess(faCatches.getAppliedAAPProcesses(),AAP_PRODUCT_PACKAGING_UNIT_QUANTITY))"),
            @Mapping(target = "resultAAPProductWeightMeasure", expression = "java(CustomMapper.getMeasureTypeFromAAPProcess(faCatches.getAppliedAAPProcesses(),AAP_PRODUCT_WEIGHT_MEASURE))"),
            @Mapping(target = "resultAAPProductPackagingUnitAverageWeightMeasure", expression = "java(CustomMapper.getMeasureTypeFromAAPProcess(faCatches.getAppliedAAPProcesses(),AAP_PRODUCT_AVERAGE_WEIGHT_MEASURE))"),
            @Mapping(target = "resultAAPProductUnitQuantity", expression = "java(CustomMapper.getMeasureTypeFromAAPProcess(faCatches.getAppliedAAPProcesses(),AAP_PRODUCT_UNIT_QUANTITY))")
    })
    FaCatchFact generateFactsForFaCatchs(FACatch faCatches);

    List<FaCatchFact> generateFactsForFaCatchs(List<FACatch> faCatches);

    @Mappings({
            @Mapping(target = "typeCodes", source = "vesselStorageCharacteristic.typeCodes")
    })
    VesselStorageCharacteristicsFact generateFactsForVesselStorageCharacteristic(VesselStorageCharacteristic vesselStorageCharacteristic);

    List<VesselStorageCharacteristicsFact> generateFactsForVesselStorageCharacteristics(List<VesselStorageCharacteristic> vesselStorageCharacteristics);

    @Mappings({
            @Mapping(target = "ids", source = "IDS")
    })
    FishingTripFact generateFactForFishingTrip(FishingTrip fishingTrip);

    List<FishingTripFact> generateFactForFishingTrips(List<FishingTrip> fishingTrip);

    @Mappings({
            @Mapping(target = "id", source = "ID"),
            @Mapping(target = "typeCode", source = "typeCode"),
            @Mapping(target = "countryID", source = "countryID"),
            @Mapping(target = "applicableFLUXCharacteristicTypeCode", source = "fluxLocation.applicableFLUXCharacteristics")

    })
    FluxLocationFact generateFactForFluxLocation(FLUXLocation fluxLocation);

    List<FluxLocationFact> generateFactsForFluxLocations(List<FLUXLocation> fluxLocation);

    @Mappings({
            @Mapping(target = "typeCode", source = "typeCode.value")
    })
    FluxCharacteristicsFact generateFactForFluxCharacteristics(FLUXCharacteristic fluxCharacteristic);

    List<FluxCharacteristicsFact> generateFactsForFluxCharacteristics(List<FLUXCharacteristic> fluxCharacteristic);

    @Mappings({
            @Mapping(target = "fishingActivityTypeCode", source = "fishingActivity.typeCode"),
            @Mapping(target = "faReportDocumentTypeCode", source = "faReportDocument.typeCode"),
            @Mapping(target = "occurrenceDateTime", source = "fishingActivity.occurrenceDateTime"),
            @Mapping(target = "reasonCode", source = "fishingActivity.reasonCode"),
            @Mapping(target = "relatedFLUXLocations", source = "fishingActivity.relatedFLUXLocations"),
            @Mapping(target = "specifiedFishingGears", source = "fishingActivity.specifiedFishingGears"),
            @Mapping(target = "specifiedFACatches", source = "fishingActivity.specifiedFACatches"),
            @Mapping(target = "specifiedFishingTrip", source = "fishingActivity.specifiedFishingTrip")
    })
    FaDepartureFact generateFactsForFaDeparture(FishingActivity fishingActivity, FAReportDocument faReportDocument);

    @Mappings({
            @Mapping(target = "fishingActivityTypeCode", source = "fishingActivity.typeCode"),
            @Mapping(target = "faReportDocumentTypeCode", source = "faReportDocument.typeCode"),
            @Mapping(target = "reasonCode", source = "fishingActivity.reasonCode"),
            @Mapping(target = "speciesTargetCode", source = "fishingActivity.speciesTargetCode"),
            @Mapping(target = "relatedFLUXLocations", source = "fishingActivity.relatedFLUXLocations")
    })
    FaEntryToSeaFact generateFactsForEntryIntoSea(FishingActivity fishingActivity, FAReportDocument faReportDocument);

    @Mappings({
            @Mapping(target = "fishingActivityTypeCode", source = "fishingActivity.typeCode"),
            @Mapping(target = "faReportDocumentTypeCode", source = "faReportDocument.typeCode"),
            @Mapping(target = "operationsQuantity", source = "fishingActivity.operationsQuantity.value"),
            @Mapping(target = "relatedFLUXLocations", source = "fishingActivity.relatedFLUXLocations")
    })
    FaFishingOperationFact generateFactsForFishingOperation(FishingActivity fishingActivity, FAReportDocument faReportDocument);

    @Mappings({
            @Mapping(target = "fishingActivityTypeCode", source = "fishingActivity.typeCode"),
            @Mapping(target = "faReportDocumentTypeCode", source = "faReportDocument.typeCode"),
            @Mapping(target = "relatedFLUXLocations", source = "fishingActivity.relatedFLUXLocations"),
    })
    FaJointFishingOperationFact generateFactsForJointFishingOperation(FishingActivity fishingActivity, FAReportDocument faReportDocument);

   @Mappings({
            @Mapping(target = "typeCode", source = "typeCode.value")
    })
   FaRelocationFact generateFactsForRelocation(FishingActivity fishingActivity);


    @Mappings({
            @Mapping(target = "typeCode", source = "typeCode.value")
    })
    FaDiscardFact generateFactsForDiscard(FishingActivity fishingActivity);


    @Mappings({
            @Mapping(target = "fishingActivityTypeCode", source = "fishingActivity.typeCode"),
            @Mapping(target = "faReportDocumentTypeCode", source = "faReportDocument.typeCode"),
            @Mapping(target = "relatedFLUXLocations", source = "fishingActivity.relatedFLUXLocations")
    })
    FaExitFromSeaFact generateFactsForExitArea(FishingActivity fishingActivity, FAReportDocument faReportDocument);

    @Mappings({
            @Mapping(target = "fishingActivityTypeCode", source = "fishingActivity.typeCode"),
            @Mapping(target = "faReportDocumentTypeCode", source = "faReportDocument.typeCode"),
            @Mapping(target = "relatedFLUXLocations", source = "fishingActivity.relatedFLUXLocations"),
            @Mapping(target = "occurrenceDateTime", source = "fishingActivity.occurrenceDateTime"),
            @Mapping(target = "reasonCode", source = "fishingActivity.reasonCode"),
            @Mapping(target = "specifiedFACatches", source = "fishingActivity.specifiedFACatches")
    })
    FaNotificationOfArrivalFact generateFactsForPriorNotificationOfArrival(FishingActivity fishingActivity, FAReportDocument faReportDocument);

    @Mappings({
            @Mapping(target = "fishingActivityTypeCode", source = "fishingActivity.typeCode"),
            @Mapping(target = "faReportDocumentTypeCode", source = "faReportDocument.typeCode"),
            @Mapping(target = "relatedFLUXLocations", source = "fishingActivity.relatedFLUXLocations"),
            @Mapping(target = "relatedVesselTransportMeans", source = "fishingActivity.relatedVesselTransportMeans"),
            @Mapping(target = "specifiedFACatches", source = "fishingActivity.specifiedFACatches"),
    })
    FaTranshipmentFact generateFactsForTranshipment(FishingActivity fishingActivity, FAReportDocument faReportDocument);


    @Mappings({
            @Mapping(target = "fishingActivityTypeCode", source = "fishingActivity.typeCode"),
            @Mapping(target = "faReportTypeCode", source = "faReportDocument.typeCode"),
            @Mapping(target = "occurrenceDateTime", source = "fishingActivity.occurrenceDateTime"),
            @Mapping(target = "reasonCode", source = "fishingActivity.reasonCode"),
            @Mapping(target = "relatedFLUXLocations", source = "fishingActivity.relatedFLUXLocations"),
            @Mapping(target = "fluxLocationTypeCodes", source = "fishingActivity.relatedFLUXLocations"),
            @Mapping(target = "fishingGearRoleCodes", source = "fishingActivity.specifiedFishingGears"),
            @Mapping(target = "fishingTripIds", source = "fishingActivity.specifiedFishingTrip.IDS")
    })
    FaDeclarationOfArrivalFact generateFactsForDeclarationOfArrival(FishingActivity fishingActivity, FAReportDocument faReportDocument);

    @Mappings({
            @Mapping(target = "id", source = "ID")
    })
    FaQueryFact generateFactsForFaQuery(FAQuery faQuery);

    @Mappings({
            @Mapping(target = "typeCode", source = "typeCode.value")
    })
    FaArrivalFact generateFactsForArrival(FishingActivity fishingActivity);

    @Mappings({
            @Mapping(target = "fishingActivityCodeType", source = "fishingActivity.typeCode"),
            @Mapping(target = "faReportDocumentTypeCode", source = "faReportDocument.typeCode"),
            @Mapping(target = "relatedFluxLocations", source = "fishingActivity.relatedFLUXLocations"),
            @Mapping(target = "specifiedFaCatchTypeCode", expression = "java(CustomMapper.getCodeTypesFromFaCatch(fishingActivity.getSpecifiedFACatches(),CODE_TYPE_FOR_FACATCH))"),
            @Mapping(target = "specifiedFaCatchFluxLocationTypeCode", expression = "java(CustomMapper.getCodeTypesFromFaCatch(fishingActivity.getSpecifiedFACatches(),CODE_TYPE_FOR_FACATCH_FLUXLOCATION))")
    })
    FaLandingFact generateFactsForLanding(FishingActivity fishingActivity, FAReportDocument faReportDocument);


    @Mappings({
            @Mapping(target = "fishingActivityTypeCode", source = "fishingActivity.typeCode"),
            @Mapping(target = "faReportDocumentTypeCode", source = "faReportDocument.typeCode"),
            @Mapping(target = "faCatchTypeCode", expression = "java(CustomMapper.getCodeTypesFromFaCatch(fishingActivity.getSpecifiedFACatches(),CODE_TYPE_FOR_FACATCH))"),
            @Mapping(target = "fluxLocationTypeCode", source = "fishingActivity.relatedFLUXLocations"),
            @Mapping(target = "vesselTransportMeansRoleCode", source = "fishingActivity.relatedVesselTransportMeans"),
            @Mapping(target = "fluxCharacteristicValueQuantity", source = "fishingActivity.specifiedFLUXCharacteristics")
    })
    FaNotificationOfTranshipmentFact generateFactsForNotificationOfTranshipment(FishingActivity fishingActivity, FAReportDocument faReportDocument);


    @Mappings({
            @Mapping(target = "typeCode", source = "typeCode.value")
    })
    FaQueryParameterFact generateFactsForFaQueryParameter(FAQueryParameter faQueryParameter);

    @Mappings({
            @Mapping(target = "referencedID", source = "FLUXResponseDocument.referencedID.value")
    })
    FaResponseFact generateFactsForFaResponse(FLUXResponseMessage fluxResponseMessage);

    @Mappings({
            @Mapping(target = "listId", source = "listID")
    })
    CodeType mapToCodeType(un.unece.uncefact.data.standard.unqualifieddatatype._20.CodeType codeType);

    @Mappings({
            @Mapping(target = "schemeId", source = "schemeID")
    })
    IdType mapToCodeType(un.unece.uncefact.data.standard.unqualifieddatatype._20.IDType idType);

    List<IdType> mapToIdType(List<un.unece.uncefact.data.standard.unqualifieddatatype._20.IDType> idTypes);

    List<CodeType> mapToCodeType(List<un.unece.uncefact.data.standard.unqualifieddatatype._20.CodeType> codeTypes);

    MeasureType mapToMeasureType(un.unece.uncefact.data.standard.unqualifieddatatype._20.MeasureType measureType);

    List<MeasureType> mapToMeasureType(List<un.unece.uncefact.data.standard.unqualifieddatatype._20.MeasureType> measureTypes);

    MeasureType mapQuantityTypeToMeasureType(un.unece.uncefact.data.standard.unqualifieddatatype._20.QuantityType quantityType);

    List<MeasureType> mapToQuantityTypeToMeasureType(List<un.unece.uncefact.data.standard.unqualifieddatatype._20.QuantityType> quantityTypes);

}
