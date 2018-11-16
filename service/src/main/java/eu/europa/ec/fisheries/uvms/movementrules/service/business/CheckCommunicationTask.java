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
package eu.europa.ec.fisheries.uvms.movementrules.service.business;

import java.util.Date;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import eu.europa.ec.fisheries.uvms.config.service.ParameterService;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.RulesServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.config.ParameterKey;
import eu.europa.ec.fisheries.uvms.movementrules.service.constants.ServiceConstants;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.PreviousReport;

public class CheckCommunicationTask implements Runnable {
    private static final long TWO_HOURS_IN_MILLISECONDS = 7200000;

    private static final Logger LOG = LoggerFactory.getLogger(CheckCommunicationTask.class);

    private RulesServiceBean rulesService;
    private ParameterService parameterService;

    CheckCommunicationTask(RulesServiceBean rulesService, ParameterService parameterService) {
        this.rulesService = rulesService;
        this.parameterService = parameterService;
    }

    public void run() {
        try {
            LOG.debug("RulesTimerBean tick");
            // Get all previous reports from DB
            List<PreviousReport> previousReports = rulesService.getPreviousMovementReports();
            long threshold = getAssetNotSendingThreshold();
            
            for (PreviousReport previousReport : previousReports) {
                Date positionTime = previousReport.getPositionTime();
                Date lastUpdated = previousReport.getUpdated();
                if (isThresholdPassed(positionTime, lastUpdated, threshold)) {
                    previousReport.setUpdated(new Date());
                    LOG.info("\t ==> Executing RULE '{}', assetGuid: {}, positionTime: {}, threshold: {}",
                            ServiceConstants.ASSET_NOT_SENDING_RULE, previousReport.getAssetGuid(), positionTime, threshold);
                    String ruleName = ServiceConstants.ASSET_NOT_SENDING_RULE;
                    rulesService.timerRuleTriggered(ruleName, previousReport);
                }
            }
        } catch (Exception e) {
            LOG.error("Could not execute 'Asset not sending' rule", e);
        }
    }
    
    private boolean isThresholdPassed(Date positionTime, Date lastUpdated, long threshold) {
        long positionThreshold = positionTime.getTime() + threshold;
        long updateThreshold = lastUpdated.getTime() + threshold;
        long now = System.currentTimeMillis();
        return positionThreshold <= now && (lastUpdated.getTime() <= positionThreshold || updateThreshold <= now);
    }
    
    private long getAssetNotSendingThreshold() {
        try {
            String thresholdSetting = parameterService.getStringValue(ParameterKey.ASSET_NOT_SENDING_THRESHOLD.getKey());
            return Long.valueOf(thresholdSetting);
        } catch (Exception e) {
            return TWO_HOURS_IN_MILLISECONDS;
        }
    }
}