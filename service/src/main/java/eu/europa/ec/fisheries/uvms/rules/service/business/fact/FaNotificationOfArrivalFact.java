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

package eu.europa.ec.fisheries.uvms.rules.service.business.fact;

import eu.europa.ec.fisheries.schema.rules.template.v1.FactType;
import eu.europa.ec.fisheries.uvms.rules.service.business.AbstractFact;
import un.unece.uncefact.data.standard.reusableaggregatebusinessinformationentity._20.FACatch;
import un.unece.uncefact.data.standard.reusableaggregatebusinessinformationentity._20.FLUXLocation;

import java.util.List;

/**
 * @author padhyad
 * @author Gregory Rinaldi
 */
public class FaNotificationOfArrivalFact extends AbstractFact {

    private CodeType fishingActivityTypeCode;

    private CodeType faReportDocumentTypeCode;

    private List<FLUXLocation> relatedFLUXLocations;

    private DateType occurrenceDateTime;

    private CodeType reasonCode;

    private List<FACatch> specifiedFACatches;

    public FaNotificationOfArrivalFact() {
        setFactType();
    }

    @Override
    public void setFactType() {
        this.factType = FactType.FA_NOTIFICATION_OF_ARRIVAL;
    }

    public CodeType getFishingActivityTypeCode() {
        return fishingActivityTypeCode;
    }

    public void setFishingActivityTypeCode(CodeType fishingActivityTypeCode) {
        this.fishingActivityTypeCode = fishingActivityTypeCode;
    }

    public CodeType getFaReportDocumentTypeCode() {
        return faReportDocumentTypeCode;
    }

    public void setFaReportDocumentTypeCode(CodeType faReportDocumentTypeCode) {
        this.faReportDocumentTypeCode = faReportDocumentTypeCode;
    }

    public List<FLUXLocation> getRelatedFLUXLocations() {
        return relatedFLUXLocations;
    }

    public void setRelatedFLUXLocations(List<FLUXLocation> relatedFLUXLocations) {
        this.relatedFLUXLocations = relatedFLUXLocations;
    }

    public DateType getOccurrenceDateTime() {
        return occurrenceDateTime;
    }

    public void setOccurrenceDateTime(DateType occurrenceDateTime) {
        this.occurrenceDateTime = occurrenceDateTime;
    }

    public CodeType getReasonCode() {
        return reasonCode;
    }

    public void setReasonCode(CodeType reasonCode) {
        this.reasonCode = reasonCode;
    }

    public List<FACatch> getSpecifiedFACatches() {
        return specifiedFACatches;
    }

    public void setSpecifiedFACatches(List<FACatch> specifiedFACatches) {
        this.specifiedFACatches = specifiedFACatches;
    }
}
