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
package eu.europa.ec.fisheries.uvms.rules.service;

import java.nio.file.AccessDeniedException;
import java.util.List;
import javax.ejb.Local;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.UpdateSubscriptionType;
import eu.europa.ec.fisheries.schema.rules.module.v1.GetAlarmListByQueryResponse;
import eu.europa.ec.fisheries.schema.rules.module.v1.GetTicketListByMovementsResponse;
import eu.europa.ec.fisheries.schema.rules.module.v1.GetTicketListByQueryResponse;
import eu.europa.ec.fisheries.schema.rules.module.v1.GetTicketsAndRulesByMovementsResponse;
import eu.europa.ec.fisheries.schema.rules.movement.v1.RawMovementType;
import eu.europa.ec.fisheries.schema.rules.search.v1.AlarmQuery;
import eu.europa.ec.fisheries.schema.rules.search.v1.TicketQuery;
import eu.europa.ec.fisheries.schema.rules.ticket.v1.TicketStatusType;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageException;
import eu.europa.ec.fisheries.uvms.rules.entity.AlarmReport;
import eu.europa.ec.fisheries.uvms.rules.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.rules.entity.PreviousReport;
import eu.europa.ec.fisheries.uvms.rules.entity.Ticket;
import eu.europa.ec.fisheries.uvms.rules.exception.DaoException;
import eu.europa.ec.fisheries.uvms.rules.exception.DaoMappingException;
import eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException;
import eu.europa.ec.fisheries.uvms.rules.exception.SearchMapperException;
import eu.europa.ec.fisheries.uvms.rules.model.exception.RulesFaultException;
import eu.europa.ec.fisheries.uvms.rules.model.exception.RulesModelException;
import eu.europa.ec.fisheries.uvms.rules.model.exception.RulesModelMapperException;
import eu.europa.ec.fisheries.uvms.rules.model.exception.RulesModelMarshallException;
import eu.europa.ec.fisheries.uvms.rules.service.exception.RulesServiceException;
import eu.europa.ec.fisheries.uvms.user.model.exception.ModelMarshallException;

@Local
public interface RulesService {

    /**
     * Creates a new custom rule
     *
     * @param customRule the rule to be added
     * @return
     * @throws RulesServiceException
     */
    CustomRule createCustomRule(CustomRule customRule, String featureName, String applicationName) throws RulesServiceException, RulesFaultException, AccessDeniedException, DaoException, RulesModelMarshallException, ModelMarshallException, MessageException;

    CustomRule updateSubscription(UpdateSubscriptionType updateSubscriptionType, String username) throws RulesServiceException, RulesFaultException, InputArgumentException, DaoException, DaoMappingException;

    /**
     * Lists (all) custom rules
     *
     * @return
     * @throws RulesServiceException
     */
//    List<CustomRuleType> getCustomRuleList() throws RulesServiceException;

    CustomRule deleteCustomRule(String guid, String username, String featureName, String applicationName) throws RulesServiceException, RulesFaultException, AccessDeniedException, DaoException, RulesModelMapperException;

    /**
     * Lists alarms by query
     *
     * @return
     * @throws RulesServiceException
     */
    GetAlarmListByQueryResponse getAlarmList(AlarmQuery query) throws RulesServiceException, RulesModelException, DaoMappingException, DaoException;

    /**
     * Lists tickets by query
     *
     * @return
     * @throws RulesServiceException
     */
    GetTicketListByQueryResponse getTicketList(String loggedInUser, TicketQuery query) throws RulesServiceException, RulesFaultException, SearchMapperException, DaoException, DaoMappingException, InputArgumentException;

    GetTicketListByMovementsResponse getTicketsByMovements(List<String> movements) throws RulesServiceException, RulesFaultException, InputArgumentException, DaoException, DaoMappingException;

    long countTicketsByMovements(List<String> movements) throws RulesServiceException, RulesFaultException, InputArgumentException, DaoException;

    /**
     * Update a ticket status
     *
     * @param ticket
     * @throws RulesServiceException
     */
    Ticket updateTicketStatus(Ticket ticket) throws RulesFaultException, InputArgumentException, DaoException, DaoMappingException;

    /**
     * Update a ticket count, for Asset not sending tickets
     *
     * @param ticket
     * @throws RulesServiceException
     */
    Ticket updateTicketCount(Ticket ticket);

    /**
     * Update an object
     *
     * @param customRuleType
     * @throws RulesServiceException
     */
    CustomRule updateCustomRule(CustomRule customRuleType, String featureName, String applicationName) throws AccessDeniedException, RulesModelMarshallException, ModelMarshallException, RulesModelException, MessageException, DaoException;

    /**
     * Update an object
     *
     * @param oldCustomRule
     * @throws RulesServiceException
     */
    CustomRule updateCustomRule(CustomRule oldCustomRule) throws RulesModelException, DaoException;

    /**
     * Creates an error report
     *
     * @param ruleName
     * @param fact
     * @throws RulesServiceException
     */
//    void createAlarmReport(String ruleName, RawMovementFact fact) throws RulesServiceException;

    /**
     * Entry point of action performed as a result of a custom rule triggered
     *
     * @param f
     *            the fact that triggered the rule
     * @param action
     *            the action(s) to be performed
     */
//    void customRuleTriggered(String ruleName, String ruleGuid, MovementFact f, String action) throws RulesServiceException;

    /**
     * Get a custom rule by guid
     *
     * @param guid
     * @return
     * @throws RulesServiceException, RulesModelMapperException, RulesFaultException
     */
    CustomRule getCustomRuleByGuid(String guid) throws RulesServiceException, RulesModelMapperException, RulesFaultException;

    AlarmReport updateAlarmStatus(AlarmReport ticket) throws RulesServiceException, RulesFaultException, DaoException, InputArgumentException;

    List<PreviousReport> getPreviousMovementReports();

    void timerRuleTriggered(String ruleName, PreviousReport previousReport);

    String reprocessAlarm(List<String> alarms, String username) throws RulesServiceException, RulesModelException, DaoMappingException, DaoException;

    void setMovementReportReceived(RawMovementType rawMovementType, String pluginType, String username) throws RulesServiceException;

    /**
     * @param guid the GUID of an alarm
     * @return an alarm
     * @throws RulesServiceException if unsuccessful
     */
    AlarmReport getAlarmReportByGuid(String guid) throws RulesServiceException, RulesFaultException, DaoException;

    /**
     * @param guid the GUID of a ticket
     * @return a ticket
     * @throws RulesServiceException if unsuccessful
     */
    Ticket getTicketByGuid(String guid) throws RulesServiceException;

    List<Ticket> updateTicketStatusByQuery(String loggedInUser, TicketQuery query, TicketStatusType status) throws RulesServiceException, RulesFaultException, InputArgumentException, DaoMappingException, DaoException, SearchMapperException;

    long getNumberOfAssetsNotSending() throws RulesServiceException, RulesFaultException;

    GetTicketsAndRulesByMovementsResponse getTicketsAndRulesByMovements(List<String> movements) throws RulesServiceException, DaoException, DaoMappingException;
}
