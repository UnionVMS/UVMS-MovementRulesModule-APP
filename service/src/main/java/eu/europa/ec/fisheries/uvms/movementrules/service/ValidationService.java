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
import eu.europa.ec.fisheries.schema.rules.module.v1.GetCustomRuleListByQueryResponse;
import eu.europa.ec.fisheries.schema.rules.search.v1.CustomRuleQuery;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.MovementFact;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.RawMovementFact;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.SanityRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.exception.DaoException;
import eu.europa.ec.fisheries.uvms.movementrules.service.exception.DaoMappingException;
import eu.europa.ec.fisheries.uvms.movementrules.service.exception.RulesServiceException;
import eu.europa.ec.fisheries.uvms.movementrules.service.exception.SearchMapperException;
import eu.europa.ec.fisheries.uvms.rules.model.exception.RulesFaultException;

@Local
public interface ValidationService {
    List<CustomRule> getCustomRulesByUser(String userName) throws RulesServiceException, RulesFaultException;

    List<CustomRule> getRunnableCustomRules() throws RulesServiceException, RulesFaultException;

    List<SanityRule> getSanityRules() throws RulesServiceException, RulesFaultException;

    GetCustomRuleListByQueryResponse getCustomRulesByQuery(CustomRuleQuery query) throws RulesServiceException, RulesFaultException, DaoMappingException, SearchMapperException, DaoException;

    // Triggered by rule engine
    void customRuleTriggered(String ruleName, String ruleGuid, MovementFact fact, String actions);

    // Triggered by rule engine
    void createAlarmReport(String ruleName, RawMovementFact fact) throws RulesServiceException;

    /**
     * @return number of open alarms
     * @throws RulesServiceException if unsuccessful
     */
    long getNumberOfOpenAlarmReports() throws RulesServiceException, RulesFaultException;

    /**
     * @return number of open tickets
     * @throws RulesServiceException if unsuccessful
     */
    long getNumberOfOpenTickets(String userName) throws RulesServiceException, RulesFaultException, DaoException;
}