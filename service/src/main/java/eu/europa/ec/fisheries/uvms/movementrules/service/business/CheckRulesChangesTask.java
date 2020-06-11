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

import java.time.Instant;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.RulesServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.ValidationServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Interval;

public class CheckRulesChangesTask implements Runnable {

    private static final Logger LOG = LoggerFactory.getLogger(CheckRulesChangesTask.class);

    ValidationServiceBean validationService;
    RulesValidator rulesValidator;
    RulesServiceBean rulesService;

    public CheckRulesChangesTask(ValidationServiceBean validationService, RulesValidator rulesValidator, RulesServiceBean rulesService) {
        this.validationService = validationService;
        this.rulesValidator = rulesValidator;
        this.rulesService = rulesService;
    }

    @Override
    public void run() {
        clearCustomRules();
    }

    private void clearCustomRules()  {
        LOG.debug("Looking outdated custom rules");
        List<CustomRule> customRules = rulesService.getRunnableCustomRules();
        boolean updateNeeded = false;
        for (CustomRule rule : customRules) {
            // If there are no time intervals, we do not need to check if the rule should be inactivated.
            boolean inactivate = !rule.getIntervals().isEmpty();
            Optional<Instant> latest = rule.getIntervals().stream().map(Interval::getEnd).max(Instant::compareTo);
            if(latest.isPresent()){
                Instant end = latest.get();
                Instant now = Instant.now();
                if (end.isAfter(now)) {
                    inactivate = false;
                }
            }
            if (inactivate) {
                LOG.debug("Inactivating {}", rule.getName());
                rule.setActive(false);
                rule.setArchived(true);
                rule.setUpdatedBy("UVMS Out of date checker");
                updateNeeded = true;
            }
        }
        if (updateNeeded) {
            LOG.debug("Clear outdated custom rules");
            rulesValidator.updateCustomRules();
        }
    }
}