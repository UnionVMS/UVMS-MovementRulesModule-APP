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

import java.nio.file.AccessDeniedException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.UUID;
import javax.ejb.Stateless;
import javax.enterprise.event.Event;
import javax.inject.Inject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import eu.europa.ec.fisheries.schema.movementrules.alarm.v1.AlarmReportType;
import eu.europa.ec.fisheries.schema.movementrules.alarm.v1.AlarmStatusType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.AvailabilityType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.CustomRuleType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.SubscriptionTypeType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.SubscritionOperationType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.UpdateSubscriptionType;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetAlarmListByQueryResponse;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketListByMovementsResponse;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketListByQueryResponse;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketsAndRulesByMovementsResponse;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.RawMovementType;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.AlarmListCriteria;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.AlarmQuery;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.AlarmSearchKey;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.ListPagination;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.TicketQuery;
import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketStatusType;
import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketType;
import eu.europa.ec.fisheries.schema.movementrules.ticketrule.v1.TicketAndRuleType;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageException;
import eu.europa.ec.fisheries.uvms.commons.notifications.NotificationMessage;
import eu.europa.ec.fisheries.uvms.movementrules.model.constant.AuditObjectTypeEnum;
import eu.europa.ec.fisheries.uvms.movementrules.model.constant.AuditOperationEnum;
import eu.europa.ec.fisheries.uvms.movementrules.model.dto.AlarmListResponseDto;
import eu.europa.ec.fisheries.uvms.movementrules.model.exception.RulesFaultException;
import eu.europa.ec.fisheries.uvms.movementrules.model.exception.RulesModelException;
import eu.europa.ec.fisheries.uvms.movementrules.model.exception.RulesModelMapperException;
import eu.europa.ec.fisheries.uvms.movementrules.model.exception.RulesModelMarshallException;
import eu.europa.ec.fisheries.uvms.movementrules.service.RulesService;
import eu.europa.ec.fisheries.uvms.movementrules.service.boundary.AuditServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.boundary.UserServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.RulesValidator;
import eu.europa.ec.fisheries.uvms.movementrules.service.constants.ServiceConstants;
import eu.europa.ec.fisheries.uvms.movementrules.service.dao.RulesDao;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.AlarmReport;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.PreviousReport;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.RuleSubscription;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Ticket;
import eu.europa.ec.fisheries.uvms.movementrules.service.event.AlarmReportCountEvent;
import eu.europa.ec.fisheries.uvms.movementrules.service.event.AlarmReportEvent;
import eu.europa.ec.fisheries.uvms.movementrules.service.event.TicketCountEvent;
import eu.europa.ec.fisheries.uvms.movementrules.service.event.TicketEvent;
import eu.europa.ec.fisheries.uvms.movementrules.service.event.TicketUpdateEvent;
import eu.europa.ec.fisheries.uvms.movementrules.service.exception.DaoException;
import eu.europa.ec.fisheries.uvms.movementrules.service.exception.DaoMappingException;
import eu.europa.ec.fisheries.uvms.movementrules.service.exception.RulesServiceException;
import eu.europa.ec.fisheries.uvms.movementrules.service.exception.SearchMapperException;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.AlarmMapper;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.CustomRuleMapper;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.TicketMapper;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.search.AlarmSearchFieldMapper;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.search.AlarmSearchValue;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.search.TicketSearchFieldMapper;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.search.TicketSearchValue;
import eu.europa.ec.fisheries.uvms.user.model.exception.ModelMarshallException;
import eu.europa.ec.fisheries.wsdl.user.types.Feature;
import eu.europa.ec.fisheries.wsdl.user.types.UserContext;

@Stateless
public class RulesServiceBean implements RulesService {

    private static final Logger LOG = LoggerFactory.getLogger(RulesServiceBean.class);

    @Inject
    private RulesValidator rulesValidator;
    
    @Inject
    private MovementReportProcessorBean movementReportBean;

    @Inject
    private UserServiceBean userService;
    
    @Inject
    private RulesDao rulesDao;

    @Inject
    private AuditServiceBean auditService;
    
    @Inject
    @AlarmReportEvent
    private Event<NotificationMessage> alarmReportEvent;

    @Inject
    @TicketEvent
    private Event<NotificationMessage> ticketEvent;

    @Inject
    @TicketUpdateEvent
    private Event<NotificationMessage> ticketUpdateEvent;

    @Inject
    @AlarmReportCountEvent
    private Event<NotificationMessage> alarmReportCountEvent;

    @Inject
    @TicketCountEvent
    private Event<NotificationMessage> ticketCountEvent;

    /**
     * {@inheritDoc}
     *
     * @param customRule
     * @throws RulesServiceException
     * @throws RulesFaultException
     */
    @Override
    public CustomRule createCustomRule(CustomRule customRule, String featureName, String applicationName) throws RulesServiceException, AccessDeniedException, RulesModelMarshallException, ModelMarshallException, MessageException {
        // Get organisation of user
        String organisationName = userService.getOrganisationName(customRule.getUpdatedBy());
        if (organisationName != null) {
            customRule.setOrganisation(organisationName);
        } else {
            LOG.warn("User {} is not connected to any organisation!", customRule.getUpdatedBy());
        }
        if (customRule.getAvailability().equals(AvailabilityType.GLOBAL.value())) {
            UserContext userContext = userService.getFullUserContext(customRule.getUpdatedBy(), applicationName);
            if (!hasFeature(userContext, featureName)) {
                throw new AccessDeniedException("Forbidden access");
            }
        }

        List<RuleSubscription> subscriptionEntities = new ArrayList<>();
        RuleSubscription creatorSubscription = new RuleSubscription();
        creatorSubscription.setCustomRule(customRule);
        creatorSubscription.setOwner(customRule.getUpdatedBy());
        creatorSubscription.setType(SubscriptionTypeType.TICKET.value());
        subscriptionEntities.add(creatorSubscription);
        customRule.getRuleSubscriptionList().addAll(subscriptionEntities);


        customRule.setUpdated(new Date());
        customRule.setStartDate(new Date());
        customRule.setGuid(UUID.randomUUID().toString());

        rulesDao.createCustomRule(customRule);

        // TODO: Rewrite so rules are loaded when changed
        rulesValidator.updateCustomRules();
        auditService.sendAuditMessage(AuditObjectTypeEnum.CUSTOM_RULE, AuditOperationEnum.CREATE, customRule.getGuid(), null, customRule.getUpdatedBy());
        return customRule;


    }

    /**
     * {@inheritDoc}
     *
     * @param guid
     * @return
     * @throws RulesServiceException
     */
    @Override
    public CustomRule getCustomRuleByGuid(String guid) throws RulesServiceException {
        try {
            return rulesDao.getCustomRuleByGuid(guid);
        } catch (DaoException e) {
            LOG.error("[ERROR] Error when getting CustomRule by GUID ] {}", e.getMessage());
            throw new RulesServiceException(e.getMessage(), e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @param oldCustomRule
     * @throws MessageException 
     * @throws RulesModelMarshallException 
     * @throws ModelMarshallException 
     * @throws AccessDeniedException 
     * @throws RulesServiceException
     * @throws DaoException 
     */
    @Override
    public CustomRule updateCustomRule(CustomRule oldCustomRule, String featureName, String applicationName) throws ModelMarshallException, RulesModelMarshallException, MessageException, AccessDeniedException, RulesServiceException, DaoException {
        // Get organisation of user
        String organisationName = userService.getOrganisationName(oldCustomRule.getUpdatedBy());
        if (organisationName != null) {
            oldCustomRule.setOrganisation(organisationName);
        } else {
            LOG.warn("User {} is not connected to any organisation!", oldCustomRule.getUpdatedBy());
        }

        if (oldCustomRule.getAvailability().equals(AvailabilityType.GLOBAL.value())) {
            UserContext userContext = userService.getFullUserContext(oldCustomRule.getUpdatedBy(), applicationName);
            if (!hasFeature(userContext, featureName)) {
                throw new AccessDeniedException("Forbidden access");
            }
        }

        CustomRule customRule = internalUpdateCustomRule(oldCustomRule);
        rulesValidator.updateCustomRules();
        auditService.sendAuditMessage(AuditObjectTypeEnum.CUSTOM_RULE, AuditOperationEnum.UPDATE, customRule.getGuid(), null, oldCustomRule.getUpdatedBy());
        return customRule;

    }

    private CustomRule internalUpdateCustomRule(CustomRule newEntity) throws DaoException {
        if (newEntity == null) {
            throw new IllegalArgumentException("Custom Rule is null");
        }

        if (newEntity.getGuid() == null) {
            throw new IllegalArgumentException("GUID of Custom Rule is null");
        }

        CustomRule oldEntity = rulesDao.getCustomRuleByGuid(newEntity.getGuid());
        newEntity.setGuid(UUID.randomUUID().toString());

        // Copy last triggered if entities are equal
        if (oldEntity.equals(newEntity)) {
            newEntity.setTriggered(oldEntity.getTriggered());
        }

        // Close old version
        oldEntity.setArchived(true);
        oldEntity.setActive(false);
        oldEntity.setEndDate(new Date());
        // Copy subscription list (ignore if provided)
        // Copy to new array to avoid concurrent modification exception
        List<RuleSubscription> subscriptions = new ArrayList<>(oldEntity.getRuleSubscriptionList());
        for (RuleSubscription subscription : subscriptions) {
            rulesDao.detachSubscription(subscription);
            newEntity.getRuleSubscriptionList().add(subscription);
            subscription.setCustomRule(newEntity);
        }

        newEntity.setUpdated(new Date());
        newEntity.setStartDate(new Date());
        newEntity = rulesDao.createCustomRule(newEntity);
        return newEntity;
    }
    /**
     * {@inheritDoc}
     *
     * @param oldCustomRule
     * @throws DaoException 
     * @throws RulesServiceException
     */
    @Override
    public CustomRule updateCustomRule(CustomRule oldCustomRule) throws DaoException {
        CustomRule updatedCustomRule = internalUpdateCustomRule(oldCustomRule);
        auditService.sendAuditMessage(AuditObjectTypeEnum.CUSTOM_RULE, AuditOperationEnum.UPDATE, updatedCustomRule.getGuid(), null, oldCustomRule.getUpdatedBy());
        return updatedCustomRule;

    }

    /**
     * {@inheritDoc}
     *
     * @param updateSubscriptionType
     */
    @Override
    public CustomRule updateSubscription(UpdateSubscriptionType updateSubscriptionType, String username) throws DaoException {
        if (updateSubscriptionType == null) {
            throw new IllegalArgumentException("Subscription is null");
        }

        boolean validRequest = updateSubscriptionType.getSubscription().getType() != null && updateSubscriptionType.getSubscription().getOwner() != null;
        if (!validRequest) {
            throw new IllegalArgumentException("Not a valid subscription!");
        }

        if (updateSubscriptionType.getRuleGuid() == null) {
            throw new IllegalArgumentException("Custom Rule GUID for Subscription is null");
        }

        CustomRule customRuleEntity = rulesDao.getCustomRuleByGuid(updateSubscriptionType.getRuleGuid());

        if (SubscritionOperationType.ADD.equals(updateSubscriptionType.getOperation())) {
            RuleSubscription ruleSubscription = new RuleSubscription();
            ruleSubscription.setOwner(updateSubscriptionType.getSubscription().getOwner());
            if (updateSubscriptionType.getSubscription().getType() != null) {
                ruleSubscription.setType(updateSubscriptionType.getSubscription().getType().name());
            }
            customRuleEntity.getRuleSubscriptionList().add(ruleSubscription);
            ruleSubscription.setCustomRule(customRuleEntity);

            // TODO: Don't log rule guid, log subscription guid?
            auditService.sendAuditMessage(AuditObjectTypeEnum.CUSTOM_RULE_SUBSCRIPTION, AuditOperationEnum.CREATE, updateSubscriptionType.getRuleGuid(), updateSubscriptionType.getSubscription().getOwner() + "/" + updateSubscriptionType.getSubscription().getType(), username);
        } else if (SubscritionOperationType.REMOVE.equals(updateSubscriptionType.getOperation())) {
            List<RuleSubscription> subscriptions = customRuleEntity.getRuleSubscriptionList();
            for (RuleSubscription subscription : subscriptions) {
                if (subscription.getOwner().equals(updateSubscriptionType.getSubscription().getOwner()) && subscription.getType().equals(updateSubscriptionType.getSubscription().getType().name())) {
                    customRuleEntity.getRuleSubscriptionList().remove(subscription);
                    rulesDao.removeSubscription(subscription);

                    // TODO: Don't log rule guid, log subscription guid?
                    auditService.sendAuditMessage(AuditObjectTypeEnum.CUSTOM_RULE_SUBSCRIPTION, AuditOperationEnum.DELETE, updateSubscriptionType.getRuleGuid(), updateSubscriptionType.getSubscription().getOwner() + "/" + updateSubscriptionType.getSubscription().getType(), username);                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
                    break;
                }
            }
        }

        return customRuleEntity;
    }

    /**
     * {@inheritDoc}
     *
     * @param guid
     * @throws RulesServiceException
     */
    @Override
    public CustomRule deleteCustomRule(String guid, String username, String featureName, String applicationName) throws RulesServiceException, AccessDeniedException, DaoException, RulesModelMapperException {
        LOG.info("[INFO] Deleting custom rule by guid: {}.", guid);
        if (guid == null) {
            throw new IllegalArgumentException("No custom rule to remove");
        }

        CustomRule customRuleFromDb = getCustomRuleByGuid(guid);
        if (customRuleFromDb.getAvailability().equals(AvailabilityType.GLOBAL.value())) {
            UserContext userContext = userService.getFullUserContext(username, applicationName);
            if (!hasFeature(userContext, featureName)) {
                throw new AccessDeniedException("Forbidden access");
            }
        }

        CustomRule entity = rulesDao.getCustomRuleByGuid(guid);
        entity.setArchived(true);
        entity.setActive(false);
        entity.setEndDate(new Date());

        rulesValidator.updateCustomRules();
        auditService.sendAuditMessage(AuditObjectTypeEnum.CUSTOM_RULE, AuditOperationEnum.DELETE, entity.getGuid(), null, username);
        return entity;

    }

    /**
     * {@inheritDoc}
     *
     * @return
     * @throws RulesServiceException
     */
    @Override
    public GetAlarmListByQueryResponse getAlarmList(AlarmQuery query) throws DaoMappingException {
        AlarmListResponseDto alarmList = getAlarmListByQuery(query);
        GetAlarmListByQueryResponse response = new GetAlarmListByQueryResponse();
        response.getAlarms().addAll(alarmList.getAlarmList());
        response.setTotalNumberOfPages(alarmList.getTotalNumberOfPages());
        response.setCurrentPage(alarmList.getCurrentPage());
        return response;

    }

    @Override
    public GetTicketListByQueryResponse getTicketList(String loggedInUser, TicketQuery query) throws DaoMappingException {
        if (query == null) {
            throw new IllegalArgumentException("Ticket list query is null");
        }
        if (query.getPagination() == null) {
            throw new IllegalArgumentException("Pagination in ticket list query is null");
        }

        Integer listSize = query.getPagination().getListSize();
        List<TicketSearchValue> searchKeyValues = TicketSearchFieldMapper.mapSearchField(query.getTicketSearchCriteria());
        List<String> validRuleGuids = rulesDao.getCustomRulesForTicketsByUser(loggedInUser);


        String sql = TicketSearchFieldMapper.createSelectSearchSql(searchKeyValues, validRuleGuids, true);
        String countSql = TicketSearchFieldMapper.createCountSearchSql(searchKeyValues, validRuleGuids, true);
        Long numberMatches = rulesDao.getTicketListSearchCount(countSql);
        List<Ticket> ticketEntityList = rulesDao.getTicketListPaginated(query.getPagination().getPage(), listSize, sql);

        List<TicketType> ticketList = new ArrayList<>();
        for (Ticket entity : ticketEntityList) {
            ticketList.add(TicketMapper.toTicketType(entity));
        }
        int numberOfPages = (int) (numberMatches / listSize);
        if (numberMatches % listSize != 0) {
            numberOfPages += 1;
        }

        GetTicketListByQueryResponse response = new GetTicketListByQueryResponse();
        response.setCurrentPage(query.getPagination().getPage());
        response.setTotalNumberOfPages(numberOfPages);
        response.getTickets().addAll(ticketList);
        return response;

    }

    @Override
    public GetTicketListByMovementsResponse getTicketsByMovements(List<String> movements) throws DaoMappingException {
        if (movements == null) {
            throw new IllegalArgumentException("Movements list is null");
        }
        if (movements.isEmpty()) {
            throw new IllegalArgumentException("Movements list is empty");
        }

        List<TicketType> ticketList = new ArrayList<>();
        List<Ticket> ticketEntityList = rulesDao.getTicketsByMovements(movements);
        for (Ticket entity : ticketEntityList) {
            ticketList.add(TicketMapper.toTicketType(entity));
        }

        GetTicketListByMovementsResponse response = new GetTicketListByMovementsResponse();
        response.getTickets().addAll(ticketList);
        return response;

    }

    @Override
    public GetTicketsAndRulesByMovementsResponse getTicketsAndRulesByMovements(List<String> movements) throws DaoException, DaoMappingException {
        List<TicketAndRuleType> ticketsAndRules = new ArrayList<>();
        // TODO: This can be done more efficiently with some join stuff
        List<Ticket> tickets = rulesDao.getTicketsByMovements(movements);
        for (Ticket ticket : tickets) {
            CustomRule rule = rulesDao.getCustomRuleByGuid(ticket.getRuleGuid());
            TicketType ticketType = TicketMapper.toTicketType(ticket);
            CustomRuleType ruleType = CustomRuleMapper.toCustomRuleType(rule);
            TicketAndRuleType ticketsAndRule = new TicketAndRuleType();
            ticketsAndRule.setTicket(ticketType);
            ticketsAndRule.setRule(ruleType);
            ticketsAndRules.add(ticketsAndRule);
        }

        GetTicketsAndRulesByMovementsResponse response = new GetTicketsAndRulesByMovementsResponse();
        response.getTicketsAndRules().addAll(ticketsAndRules);
        return response;
    }

    @Override
    public long countTicketsByMovements(List<String> movements) {
        if (movements == null) {
            throw new IllegalArgumentException("Movements list is null");
        }
        if (movements.isEmpty()) {
            throw new IllegalArgumentException("Movements list is empty");
        }

        return rulesDao.countTicketListByMovements(movements);
    }

    @Override
    public Ticket updateTicketStatus(Ticket ticket) {
        if (ticket == null || ticket.getGuid() == null) {
            throw new IllegalArgumentException("Ticket is null");
        }
        Ticket entity = rulesDao.getTicketByGuid(ticket.getGuid());

        entity.setStatus(ticket.getStatus());
        entity.setUpdated(new Date());
        entity.setUpdatedBy(ticket.getUpdatedBy());

        rulesDao.updateTicket(entity);

        // Notify long-polling clients of the update
        ticketUpdateEvent.fire(new NotificationMessage("guid", entity.getGuid()));
        // Notify long-polling clients of the change (no value since FE will need to fetch it)
        ticketCountEvent.fire(new NotificationMessage("ticketCount", null));
        auditService.sendAuditMessage(AuditObjectTypeEnum.TICKET, AuditOperationEnum.UPDATE, entity.getGuid(), "", ticket.getUpdatedBy());
        return entity;

    }

    @Override
    public List<Ticket> updateTicketStatusByQuery(String loggedInUser, TicketQuery query, TicketStatusType status) {
        if (loggedInUser == null) {
            throw new IllegalArgumentException("LoggedInUser is null, can not update status");
        }
        if (status == null) {
            throw new IllegalArgumentException("Status is null, can not update status");
        }
        if (query == null) {
            throw new IllegalArgumentException("Status is null, can not update status");
        }
        List<TicketSearchValue> searchKeyValues = TicketSearchFieldMapper.mapSearchField(query.getTicketSearchCriteria());
        List<String> validRuleGuids = rulesDao.getCustomRulesForTicketsByUser(loggedInUser);
        String sql = TicketSearchFieldMapper.createSelectSearchSql(searchKeyValues, validRuleGuids, true);
        List<Ticket> tickets = rulesDao.getTicketList(sql);
        for (Ticket ticket : tickets) {
            ticket.setStatus(status.name());
            ticket.setUpdated(new Date());
            ticket.setUpdatedBy(loggedInUser);

            rulesDao.updateTicket(ticket);

            // Notify long-polling clients of the update
            ticketUpdateEvent.fire(new NotificationMessage("guid", ticket.getGuid()));
            auditService.sendAuditMessage(AuditObjectTypeEnum.TICKET, AuditOperationEnum.UPDATE, ticket.getGuid(), null, loggedInUser);
        }

        // Notify long-polling clients of the change (no value since FE will need to fetch it)
        ticketCountEvent.fire(new NotificationMessage("ticketCount", null));
        return tickets;

    }

    @Override
    public long getNumberOfAssetsNotSending() {
        return rulesDao.getNumberOfTicketsByRuleGuid(ServiceConstants.ASSET_NOT_SENDING_RULE);
    }

    @Override
    public AlarmReport updateAlarmStatus(AlarmReport alarm) throws DaoException {
        AlarmReport entity = rulesDao.getAlarmReportByGuid(alarm.getGuid());
        if (entity == null) {
            throw new IllegalArgumentException("Alarm is null", null);
        }

        entity.setStatus(alarm.getStatus());
        entity.setUpdatedBy(alarm.getUpdatedBy());
        entity.setUpdated(new Date());
        if (entity.getRawMovement() != null) {
            entity.getRawMovement().setActive(!alarm.getRawMovement().getActive()); /*isInactivatePosition()*/
        }

        rulesDao.updateAlarm(entity);

        // Notify long-polling clients of the change
        alarmReportEvent.fire(new NotificationMessage("guid", entity.getGuid()));
        // Notify long-polling clients of the change (no vlaue since FE will need to fetch it)
        alarmReportCountEvent.fire(new NotificationMessage("alarmCount", null));
        auditService.sendAuditMessage(AuditObjectTypeEnum.ALARM, AuditOperationEnum.UPDATE, entity.getGuid(), null, alarm.getUpdatedBy());
        return entity;
    }

    // Triggered by RulesTimerBean
    @Override
    public List<PreviousReport> getPreviousMovementReports() {
        return rulesDao.getPreviousReportList();
    }

    // Triggered by timer rule
    @Override
    public void timerRuleTriggered(String ruleName, PreviousReport previousReport) {
        LOG.info("Timer rule triggered for asset: {}", previousReport.getAssetGuid());
        // Check if ticket already is created for this asset
        Ticket ticketEntity = rulesDao.getTicketByAssetAndRule(previousReport.getAssetGuid(), ruleName); // ruleName gets renamed

        if (ticketEntity == null) {
            createAssetNotSendingTicket(ruleName, previousReport);
        } else if (ticketEntity.getTicketCount() != null) {
            ticketEntity.setTicketCount(ticketEntity.getTicketCount() + 1);
            updateTicketCount(ticketEntity);
        } else {
            ticketEntity.setTicketCount(2L);
            updateTicketCount(ticketEntity);
        }
    }

    private void createAssetNotSendingTicket(String ruleName, PreviousReport previousReport) {
        Ticket ticket = new Ticket();
        ticket.setAssetGuid(previousReport.getAssetGuid());
        ticket.setCreatedDate(new Date());
        ticket.setRuleName(ruleName);
        ticket.setRuleGuid(ruleName);
        ticket.setUpdatedBy("UVMS");
        ticket.setUpdated(new Date());
        ticket.setStatus(TicketStatusType.OPEN.value());
        ticket.setTicketCount(1L);
        rulesDao.createTicket(ticket);

		auditService.sendAuditMessage(AuditObjectTypeEnum.TICKET, AuditOperationEnum.CREATE, ticket.getGuid(), null, ticket.getUpdatedBy());	        
		// Notify long-polling clients of the change
        ticketCountEvent.fire(new NotificationMessage("ticketCount", null));
    }

    @Override
    public Ticket updateTicketCount(Ticket ticket) {
        if (ticket == null || ticket.getGuid() == null) {
            throw new IllegalArgumentException("Ticket is null, can not upate status");
        }
        ticket.setUpdated(new Date());

        // Notify long-polling clients of the update
        ticketUpdateEvent.fire(new NotificationMessage("guid", ticket.getGuid()));
        // Notify long-polling clients of the change (no value since FE will need to fetch it)
        ticketCountEvent.fire(new NotificationMessage("ticketCount", null));
        auditService.sendAuditMessage(AuditObjectTypeEnum.TICKET, AuditOperationEnum.UPDATE, ticket.getGuid(), null, ticket.getUpdatedBy());
        return ticket;
    }

    @Override
    public AlarmReport getAlarmReportByGuid(String guid) throws DaoException {
        return rulesDao.getAlarmReportByGuid(guid);
    }

    @Override
    public Ticket getTicketByGuid(String guid){
        return rulesDao.getTicketByGuid(guid);
    }

    @Override
    public String reprocessAlarm(List<String> alarmGuids, String username) throws RulesServiceException, DaoMappingException, DaoException {
        AlarmQuery query = mapToOpenAlarmQuery(alarmGuids);
        AlarmListResponseDto alarms = getAlarmListByQuery(query);

        for (AlarmReportType alarm : alarms.getAlarmList()) {
            // Cannot reprocess without a movement (i.e. "Asset not sending" alarm)
            if (alarm.getRawMovement() == null) {
                continue;
            }

            // Mark the alarm as REPROCESSED before reprocessing. That will create a new alarm (if still wrong) with the items remaining.
            alarm.setStatus(AlarmStatusType.REPROCESSED);
            //its this, not change updateAlarmStatus or change the DTO, I think that this is the best of a bad lot
            alarm = AlarmMapper.toAlarmReportType(updateAlarmStatus(AlarmMapper.toAlarmReportEntity(alarm)));
            auditService.sendAuditMessage(AuditObjectTypeEnum.ALARM, AuditOperationEnum.UPDATE, alarm.getGuid(), null, username);
            RawMovementType rawMovementType = alarm.getRawMovement();
            // TODO: Use better type (some variation of PluginType...)
            String pluginType = alarm.getPluginType();
            movementReportBean.setMovementReportReceived(rawMovementType, pluginType, username);
        }
        // return RulesDataSourceResponseMapper.mapToAlarmListFromResponse(response, messageId);
        // TODO: Better
        return "OK";

    }

    private AlarmListResponseDto getAlarmListByQuery(AlarmQuery query) throws DaoMappingException {
        if (query == null) {
            throw new IllegalArgumentException("Alarm list query is null");
        }
        if (query.getPagination() == null) {
            throw new IllegalArgumentException("Pagination in alarm list query is null");
        }


        Integer page = query.getPagination().getPage();
        Integer listSize = query.getPagination().getListSize();

        List<AlarmSearchValue> searchKeyValues = AlarmSearchFieldMapper.mapSearchField(query.getAlarmSearchCriteria());

        String sql = AlarmSearchFieldMapper.createSelectSearchSql(searchKeyValues, query.isDynamic());
        String countSql = AlarmSearchFieldMapper.createCountSearchSql(searchKeyValues, query.isDynamic());

        Long numberMatches = rulesDao.getAlarmListSearchCount(countSql);
        List<AlarmReport> alarmEntityList = rulesDao.getAlarmListPaginated(page, listSize, sql);

        List<AlarmReportType> alarmList = new ArrayList<>();
        for (AlarmReport entity : alarmEntityList) {
            alarmList.add(AlarmMapper.toAlarmReportType(entity));
        }

        int numberOfPages = (int) (numberMatches / listSize);
        if (numberMatches % listSize != 0) {
            numberOfPages += 1;
        }

        AlarmListResponseDto response = new AlarmListResponseDto();
        response.setTotalNumberOfPages(numberOfPages);
        response.setCurrentPage(query.getPagination().getPage());
        response.setAlarmList(alarmList);

        return response;
    }

    private AlarmQuery mapToOpenAlarmQuery(List<String> alarmGuids) {
        AlarmQuery query = new AlarmQuery();
        ListPagination pagination = new ListPagination();
        pagination.setListSize(alarmGuids.size());
        pagination.setPage(1);
        query.setPagination(pagination);

        for (String alarmGuid : alarmGuids) {
            AlarmListCriteria criteria = new AlarmListCriteria();
            criteria.setKey(AlarmSearchKey.ALARM_GUID);
            criteria.setValue(alarmGuid);
            query.getAlarmSearchCriteria().add(criteria);
        }

        // We only want open alarms
        AlarmListCriteria openCrit = new AlarmListCriteria();
        openCrit.setKey(AlarmSearchKey.STATUS);
        openCrit.setValue(AlarmStatusType.OPEN.name());
        query.getAlarmSearchCriteria().add(openCrit);
        query.setDynamic(true);
        return query;
    }

    private boolean hasFeature(UserContext userContext, String featureName) {
        for (eu.europa.ec.fisheries.wsdl.user.types.Context c : userContext.getContextSet().getContexts()) {
            for (Feature f : c.getRole().getFeature()) {
                if (featureName.equals(f.getName())) {
                    return true;
                }
            }
        }
        return false;
    }
}
