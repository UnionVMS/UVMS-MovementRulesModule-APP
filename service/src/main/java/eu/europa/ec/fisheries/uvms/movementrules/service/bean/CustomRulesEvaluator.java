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
package eu.europa.ec.fisheries.uvms.movementrules.service.bean;

import java.util.Date;
import javax.ejb.EJB;
import javax.ejb.Stateless;
import javax.inject.Inject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.MovementSourceType;
import eu.europa.ec.fisheries.uvms.config.exception.ConfigServiceException;
import eu.europa.ec.fisheries.uvms.config.service.ParameterService;
import eu.europa.ec.fisheries.uvms.movementrules.model.dto.MovementDetails;
import eu.europa.ec.fisheries.uvms.movementrules.service.boundary.SpatialRestClient;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.RulesValidator;
import eu.europa.ec.fisheries.uvms.movementrules.service.config.ParameterKey;
import eu.europa.ec.fisheries.uvms.movementrules.service.dao.RulesDao;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.PreviousReport;

@Stateless
public class CustomRulesEvaluator {

    private static final Logger LOG = LoggerFactory.getLogger(CustomRulesEvaluator.class);
    
    @Inject
    private RulesValidator rulesValidator;
    
    @EJB
    private ParameterService parameterService;
    
    @Inject
    private SpatialRestClient spatialClient;
    
    @Inject
    private RulesDao rulesDao;
    
    public void evaluate(MovementDetails movementDetails) {
        
        Long timeDiffPositionReport = timeDiffAndPersistPreviousReport(movementDetails.getSource(), movementDetails.getAssetGuid(), movementDetails.getFlagState(), movementDetails.getPositionTime());
        movementDetails.setTimeDiffPositionReport(timeDiffPositionReport);
        
        spatialClient.populateAreasAndAreaTransitions(movementDetails);
        
        rulesValidator.evaluate(movementDetails);
    }
  
    private Long timeDiffAndPersistPreviousReport(String movementSource, String assetGuid, String assetFlagState, Date positionTime) {
        // This needs to be done before persisting last report
        Long timeDiffInSeconds = null;
        Long timeDiff = timeDiffFromLastCommunication(assetGuid, positionTime);
        timeDiffInSeconds = timeDiff != null ? timeDiff / 1000 : null;

        // We only persist our own last communications that were not from AIS.
        if (isLocalFlagState(assetFlagState) && !movementSource.equals(MovementSourceType.AIS.value())) {
            persistLastCommunication(assetGuid, positionTime);
        }

        return timeDiffInSeconds;
    }
    
    
    private Long timeDiffFromLastCommunication(String assetGuid, Date thisTime) {
        Long timeDiff = null;
        try {
            PreviousReport entity = rulesDao.getPreviousReportByAssetGuid(assetGuid);

            Date previousTime = entity.getPositionTime();
            timeDiff = thisTime.getTime() - previousTime.getTime();
        } catch (Exception e) {
            // If something goes wrong, continue with the other validation
            LOG.error("[ERROR] Error when getting previous report by asset guid {}", e.getMessage());
        }
        return timeDiff;
    }
    
    private void persistLastCommunication(String assetGuid, Date positionTime) {
        PreviousReport entity = rulesDao.getPreviousReportByAssetGuid(assetGuid);
        if (entity == null) {
            entity = new PreviousReport();
        }
        entity.setPositionTime(positionTime);
        entity.setAssetGuid(assetGuid);
        entity.setUpdated(new Date());
        entity.setUpdatedBy("UVMS");
        rulesDao.updatePreviousReport(entity);
    }
    
    private boolean isLocalFlagState(String flagState) {
        try {
            String localFlagState = parameterService.getStringValue(ParameterKey.LOCAL_FLAGSTATE.getKey());
            return flagState.equalsIgnoreCase(localFlagState);
        } catch (ConfigServiceException e) {
            LOG.error("Could not get local flag state", e);
            return false;
        }
    }
}
