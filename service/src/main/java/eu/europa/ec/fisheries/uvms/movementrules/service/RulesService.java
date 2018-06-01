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

import java.nio.file.AccessDeniedException;
import java.util.List;
import javax.ejb.Local;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.UpdateSubscriptionType;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetAlarmListByQueryResponse;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketListByMovementsResponse;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketListByQueryResponse;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketsAndRulesByMovementsResponse;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.RawMovementType;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.AlarmQuery;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.TicketQuery;
import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketStatusType;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageException;
import eu.europa.ec.fisheries.uvms.movementrules.model.exception.RulesFaultException;
import eu.europa.ec.fisheries.uvms.movementrules.model.exception.RulesModelException;
import eu.europa.ec.fisheries.uvms.movementrules.model.exception.RulesModelMapperException;
import eu.europa.ec.fisheries.uvms.movementrules.model.exception.RulesModelMarshallException;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.AlarmReport;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.PreviousReport;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Ticket;
import eu.europa.ec.fisheries.uvms.movementrules.service.exception.DaoException;
import eu.europa.ec.fisheries.uvms.movementrules.service.exception.DaoMappingException;
import eu.europa.ec.fisheries.uvms.movementrules.service.exception.RulesServiceException;
import eu.europa.ec.fisheries.uvms.movementrules.service.exception.SearchMapperException;
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

    CustomRule updateSubscription(UpdateSubscriptionType updateSubscriptionType, String username) throws RulesServiceException, RulesFaultException, DaoException, DaoMappingException;

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
    GetTicketListByQueryResponse getTicketList(String loggedInUser, TicketQuery query) throws RulesServiceException, RulesFaultException, SearchMapperException, DaoException, DaoMappingException;

    GetTicketListByMovementsResponse getTicketsByMovements(List<String> movements) throws RulesServiceException, RulesFaultException, DaoException, DaoMappingException;

    long countTicketsByMovements(List<String> movements) throws RulesServiceException, RulesFaultException, DaoException;

    /**
     * Update a ticket status
     *
     * @param ticket
     * @throws RulesServiceException
     */
    Ticket updateTicketStatus(Ticket ticket) throws RulesServiceException;

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
     * @throws AccessDeniedException 
     * @throws RulesServiceException
     * @throws DaoException 
     */
    CustomRule updateCustomRule(CustomRule customRuleType, String featureName, String applicationName) throws ModelMarshallException, RulesModelMarshallException, MessageException, AccessDeniedException, RulesServiceException, DaoException;

    /**
     * Update an object
     *
     * @param oldCustomRule
     * @throws RulesServiceException
     */
    CustomRule updateCustomRule(CustomRule oldCustomRule) throws DaoException;

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

    AlarmReport updateAlarmStatus(AlarmReport ticket) throws RulesServiceException, RulesFaultException, DaoException;

    List<PreviousReport> getPreviousMovementReports();

    void timerRuleTriggered(String ruleName, PreviousReport previousReport);

    String reprocessAlarm(List<String> alarms, String username) throws RulesServiceException, RulesModelException, DaoMappingException, DaoException;

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

    List<Ticket> updateTicketStatusByQuery(String loggedInUser, TicketQuery query, TicketStatusType status) throws RulesServiceException, RulesFaultException, DaoMappingException, DaoException, SearchMapperException;

    long getNumberOfAssetsNotSending() throws RulesServiceException, RulesFaultException;

    GetTicketsAndRulesByMovementsResponse getTicketsAndRulesByMovements(List<String> movements) throws RulesServiceException, DaoException, DaoMappingException;
}
