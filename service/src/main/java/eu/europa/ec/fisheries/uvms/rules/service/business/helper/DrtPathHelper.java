/*
Developed by the European Commission - Directorate General for Maritime Affairs and Fisheries @ European Union, 2015-2016.

This file is part of the Integrated Fisheries Data Management (IFDM) Suite. The IFDM Suite is free software: you can redistribute it
and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of
the License, or any later version. The IFDM Suite is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details. You should have received a copy of the GNU General Public License along with the IFDM Suite. If not, see <http://www.gnu.org/licenses/>.

*/
package eu.europa.ec.fisheries.uvms.rules.service.business.helper;

import eu.europa.ec.fisheries.schema.rules.template.v1.FactType;
import org.drools.core.util.StringUtils;

/**
 * Created by kovian on 18/07/2017.
 */
public enum DrtPathHelper {

    FA_DEPARTURE("/templates/FaDeparture.drt"),
    FA_ARRIVAL("/templates/FaArrival.drt"),
    FA_CATCH("/templates/FaCatch.drt"),
    FA_DISCARD("/templates/FaDiscard.drt"),
    FA_ENTRY_TO_SEA("/templates/FaEntryToSea.drt"),
    FA_EXIT_FROM_SEA("/templates/FaExitFromSea.drt"),
    FA_FISHING_OPERATION("/templates/FaFishingOperation.drt"),
    FA_JOINT_FISHING_OPERATION("/templates/FaJointFishingOperation.drt"),
    FA_LANDING("/templates/FaLanding.drt"),
    FA_NOTIFICATION_OF_ARRIVAL("/templates/FaNotificationOfArrival.drt"),
    FA_NOTIFICATION_OF_TRANSHIPMENT("/templates/FaNotificationOfTranshipment.drt"),
    FA_REPORT_DOCUMENT("/templates/FaReportDocument.drt"),
    FISHING_ACTIVITY("/templates/FishingActivity.drt"),
    FA_QUERY("/templates/FaQuery.drt"),
    FA_QUERY_PARAMETER("/templates/FaQueryParameter.drt"),
    FA_RELOCATION("/templates/FaRelocation.drt"),
    FA_RESPONSE("/templates/FaResponse.drt"),
    FA_TRANSHIPMENT("/templates/FaTranshipment.drt"),
    FISHING_GEAR("/templates/FishingGear.drt"),
    FISHING_TRIP("/templates/FishingTrip.drt"),
    FLUX_CHARACTERISTIC("/templates/FluxCharacteristics.drt"),
    FLUX_FA_REPORT_MESSAGE("/templates/FluxFaReportMessage.drt"),
    FLUX_LOCATION("/templates/FluxLocation.drt"),
    GEAR_CHARACTERISTIC("/templates/GearCharacteristics.drt"),
    GEAR_PROBLEM("/templates/GearProblem.drt"),
    STRUCTURED_ADDRESS("/templates/StructuredAddress.drt"),
    VESSEL_STORAGE_CHARACTERISTIC("/templates/VesselStorageCharacteristics.drt"),
    VESSEL_TRANSPORT_MEANS("/templates/VesselTransportMeans.drt"),
    FA_VALIDATION_QUALITY_ANALYSIS("/templates/FaValidationQualityAnalysis.drt");
    
    private String path;

    DrtPathHelper(String PATH) {
        this.path = PATH;
    }

    public String getPath() {
        return path;
    }

    public static String getDrtPath(FactType factType) {
        try {
            return valueOf(factType.name()).getPath();
        } catch (IllegalArgumentException e) {
            return StringUtils.EMPTY;
        }
    }
}
