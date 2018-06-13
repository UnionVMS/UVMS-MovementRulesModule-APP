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
package eu.europa.ec.fisheries.uvms.movementrules.service;

import java.util.List;
import javax.ejb.Local;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.CustomRuleQuery;
import eu.europa.ec.fisheries.uvms.movementrules.model.exception.MovementRulesFaultException;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.MovementFact;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.RawMovementFact;
import eu.europa.ec.fisheries.uvms.movementrules.service.dto.CustomRuleListResponseDto;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.SanityRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.exception.RulesServiceException;

@Local
public interface ValidationService {
    List<CustomRule> getCustomRulesByUser(String userName) throws RulesServiceException, MovementRulesFaultException;

    List<CustomRule> getRunnableCustomRules() throws RulesServiceException, MovementRulesFaultException;

    List<SanityRule> getSanityRules() throws RulesServiceException, MovementRulesFaultException;

    CustomRuleListResponseDto getCustomRulesByQuery(CustomRuleQuery query) throws RulesServiceException, MovementRulesFaultException;

    // Triggered by rule engine
    void customRuleTriggered(String ruleName, String ruleGuid, MovementFact fact, String actions);

    // Triggered by rule engine
    void createAlarmReport(String ruleName, RawMovementFact fact) throws RulesServiceException;

    /**
     * @return number of open alarms
     * @throws RulesServiceException if unsuccessful
     */
    long getNumberOfOpenAlarmReports() throws RulesServiceException, MovementRulesFaultException;

    /**
     * @return number of open tickets
     * @throws RulesServiceException if unsuccessful
     */
    long getNumberOfOpenTickets(String userName) throws RulesServiceException, MovementRulesFaultException;
}
