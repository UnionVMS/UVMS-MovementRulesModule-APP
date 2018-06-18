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
import eu.europa.ec.fisheries.uvms.movementrules.model.exception.MovementRulesModelException;
import eu.europa.ec.fisheries.uvms.movementrules.service.RulesService;
import eu.europa.ec.fisheries.uvms.movementrules.service.ValidationService;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Interval;
import eu.europa.ec.fisheries.uvms.movementrules.service.exception.RulesServiceException;

public class CheckRulesChangesTask implements Runnable {

    private static final Logger LOG = LoggerFactory.getLogger(CheckRulesChangesTask.class);

    ValidationService validationService;
    RulesValidator rulesValidator;
    RulesService rulesService;

    public CheckRulesChangesTask(ValidationService validationService, RulesValidator rulesValidator, RulesService rulesService) {
        this.validationService = validationService;
        this.rulesValidator = rulesValidator;
        this.rulesService = rulesService;
    }

    @Override
    public void run() {
        clearCustomRules();
        LOG.debug("Checking for changes in sanity rules");
        rulesValidator.updateSanityRules();
    }

    private void clearCustomRules() {
        LOG.debug("Looking outdated custom rules");
        try {
            List<CustomRule> customRules = validationService.getRunnableCustomRules();
            boolean updateNeeded = false;
            for (CustomRule rule : customRules) {
                // If there are no time intervals, we do not need to check if the rule should be inactivated.
                boolean inactivate = !rule.getIntervals().isEmpty();
                for (Interval interval : rule.getIntervals()) {
                    if (interval.getEnd() != null && interval.getStart() != null) {
                        Date end = interval.getEnd();
                        Date start = interval.getStart();
                        Date now = new Date();
                        if (start.before(now) && end.after(now)) {
                            inactivate = false;
                            break;
                        }
                    }
                }
                if (inactivate) {
                    LOG.debug("Inactivating {}", rule.getName());
                    rule.setActive(false);
                    rule.setUpdatedBy("UVMS");
                    rulesService.updateCustomRule(rule);
                    updateNeeded = true;
                }
            }
            if (updateNeeded) {
                LOG.debug("Clear outdated custom rules");
                rulesValidator.updateCustomRules();
            }
        } catch (RulesServiceException | MovementRulesModelException e) {
            LOG.error("[ Error when getting sanity rules ]");
            // TODO: Throw exception???
        }
    }
}