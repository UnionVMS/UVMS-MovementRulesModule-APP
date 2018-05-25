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
package eu.europa.ec.fisheries.uvms.rules.service.bean;

import java.nio.file.AccessDeniedException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.FutureTask;
import javax.ejb.EJB;
import javax.ejb.Stateless;
import javax.enterprise.event.Event;
import javax.inject.Inject;
import javax.jms.JMSException;


import eu.europa.ec.fisheries.uvms.rules.constant.UvmsConstants;
import eu.europa.ec.fisheries.uvms.rules.entity.*;
import eu.europa.ec.fisheries.uvms.rules.exception.SearchMapperException;
import eu.europa.ec.fisheries.uvms.rules.mapper.PreviousReportMapper;
import eu.europa.ec.fisheries.uvms.rules.mapper.search.AlarmSearchFieldMapper;
import eu.europa.ec.fisheries.uvms.rules.mapper.search.AlarmSearchValue;
import eu.europa.ec.fisheries.uvms.rules.mapper.search.TicketSearchFieldMapper;
import eu.europa.ec.fisheries.uvms.rules.mapper.search.TicketSearchValue;
import eu.europa.ec.fisheries.uvms.rules.model.exception.RulesModelMarshallException;
import eu.europa.ec.fisheries.uvms.user.model.exception.ModelMarshallException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import eu.europa.ec.fisheries.schema.exchange.movement.v1.MovementRefTypeType;
import eu.europa.ec.fisheries.schema.exchange.plugin.types.v1.PluginType;
import eu.europa.ec.fisheries.schema.mobileterminal.types.v1.ComChannelAttribute;
import eu.europa.ec.fisheries.schema.mobileterminal.types.v1.ComChannelType;
import eu.europa.ec.fisheries.schema.mobileterminal.types.v1.MobileTerminalType;
import eu.europa.ec.fisheries.schema.movement.v1.MovementType;
import eu.europa.ec.fisheries.schema.rules.alarm.v1.AlarmReportType;
import eu.europa.ec.fisheries.schema.rules.alarm.v1.AlarmStatusType;
import eu.europa.ec.fisheries.schema.rules.asset.v1.AssetId;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.AvailabilityType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.CustomRuleType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.SubscriptionTypeType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.SubscritionOperationType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.UpdateSubscriptionType;
import eu.europa.ec.fisheries.schema.rules.mobileterminal.v1.IdList;
import eu.europa.ec.fisheries.schema.rules.module.v1.GetTicketsAndRulesByMovementsResponse;
import eu.europa.ec.fisheries.schema.rules.movement.v1.MovementSourceType;
import eu.europa.ec.fisheries.schema.rules.movement.v1.RawMovementType;
import eu.europa.ec.fisheries.schema.rules.previous.v1.PreviousReportType;
import eu.europa.ec.fisheries.schema.rules.search.v1.AlarmListCriteria;
import eu.europa.ec.fisheries.schema.rules.search.v1.AlarmQuery;
import eu.europa.ec.fisheries.schema.rules.search.v1.AlarmSearchKey;
import eu.europa.ec.fisheries.schema.rules.search.v1.ListPagination;
import eu.europa.ec.fisheries.schema.rules.search.v1.TicketQuery;
import eu.europa.ec.fisheries.schema.rules.source.v1.GetAlarmListByQueryResponse;
import eu.europa.ec.fisheries.schema.rules.source.v1.GetTicketListByMovementsResponse;
import eu.europa.ec.fisheries.schema.rules.source.v1.GetTicketListByQueryResponse;
import eu.europa.ec.fisheries.schema.rules.ticket.v1.TicketStatusType;
import eu.europa.ec.fisheries.schema.rules.ticket.v1.TicketType;
import eu.europa.ec.fisheries.schema.rules.ticketrule.v1.TicketAndRuleType;
import eu.europa.ec.fisheries.uvms.asset.model.exception.AssetModelMapperException;
import eu.europa.ec.fisheries.uvms.commons.date.DateUtils;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageException;
import eu.europa.ec.fisheries.uvms.commons.notifications.NotificationMessage;
import eu.europa.ec.fisheries.uvms.mobileterminal.model.exception.MobileTerminalModelMapperException;
import eu.europa.ec.fisheries.uvms.mobileterminal.model.exception.MobileTerminalUnmarshallException;
import eu.europa.ec.fisheries.uvms.rules.dao.RulesDao;
import eu.europa.ec.fisheries.uvms.rules.exception.DaoException;
import eu.europa.ec.fisheries.uvms.rules.exception.DaoMappingException;
import eu.europa.ec.fisheries.uvms.rules.mapper.AlarmMapper;
import eu.europa.ec.fisheries.uvms.rules.mapper.CustomRuleMapper;
import eu.europa.ec.fisheries.uvms.rules.mapper.TicketMapper;
import eu.europa.ec.fisheries.uvms.rules.model.constant.AuditObjectTypeEnum;
import eu.europa.ec.fisheries.uvms.rules.model.constant.AuditOperationEnum;
import eu.europa.ec.fisheries.uvms.rules.model.dto.AlarmListResponseDto;
import eu.europa.ec.fisheries.uvms.rules.model.dto.TicketListResponseDto;
import eu.europa.ec.fisheries.uvms.rules.model.exception.RulesFaultException;
import eu.europa.ec.fisheries.uvms.rules.model.exception.RulesModelException;
import eu.europa.ec.fisheries.uvms.rules.model.exception.RulesModelMapperException;
import eu.europa.ec.fisheries.uvms.rules.service.RulesService;
import eu.europa.ec.fisheries.uvms.rules.service.boundary.AssetServiceBean;
import eu.europa.ec.fisheries.uvms.rules.service.boundary.AuditServiceBean;
import eu.europa.ec.fisheries.uvms.rules.service.boundary.ConfigServiceBean;
import eu.europa.ec.fisheries.uvms.rules.service.boundary.ExchangeServiceBean;
import eu.europa.ec.fisheries.uvms.rules.service.boundary.MobileTerminalServiceBean;
import eu.europa.ec.fisheries.uvms.rules.service.boundary.MovementServiceBean;
import eu.europa.ec.fisheries.uvms.rules.service.boundary.UserServiceBean;
import eu.europa.ec.fisheries.uvms.rules.service.business.MovementFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.PreviousReportFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.RawMovementFact;
import eu.europa.ec.fisheries.uvms.rules.service.business.RulesUtil;
import eu.europa.ec.fisheries.uvms.rules.service.business.RulesValidator;
import eu.europa.ec.fisheries.uvms.rules.service.event.AlarmReportCountEvent;
import eu.europa.ec.fisheries.uvms.rules.service.event.AlarmReportEvent;
import eu.europa.ec.fisheries.uvms.rules.service.event.TicketCountEvent;
import eu.europa.ec.fisheries.uvms.rules.service.event.TicketEvent;
import eu.europa.ec.fisheries.uvms.rules.service.event.TicketUpdateEvent;
import eu.europa.ec.fisheries.uvms.rules.service.exception.InputArgumentException;
import eu.europa.ec.fisheries.uvms.rules.service.exception.RulesServiceException;
import eu.europa.ec.fisheries.uvms.rules.service.mapper.AssetAssetIdMapper;
import eu.europa.ec.fisheries.uvms.rules.service.mapper.MobileTerminalMapper;
import eu.europa.ec.fisheries.uvms.rules.service.mapper.MovementFactMapper;
import eu.europa.ec.fisheries.uvms.rules.service.mapper.RawMovementFactMapper;
import eu.europa.ec.fisheries.wsdl.asset.group.AssetGroup;
import eu.europa.ec.fisheries.wsdl.asset.types.Asset;
import eu.europa.ec.fisheries.wsdl.user.types.Feature;
import eu.europa.ec.fisheries.wsdl.user.types.UserContext;

@Stateless
public class RulesServiceBean implements RulesService {

    private static final Logger LOG = LoggerFactory.getLogger(RulesServiceBean.class);

    @EJB
    private RulesValidator rulesValidator;

    @Inject
    private UserServiceBean userService;
    
    @Inject
    private AssetServiceBean assetService;
    
    @Inject
    private MobileTerminalServiceBean mobileTerminalService;

    @EJB
    private RulesDao rulesDao;

    @Inject
    private MovementServiceBean movementService;
    
    @Inject
    private ExchangeServiceBean exchangeService;
    
    @Inject
    private AuditServiceBean auditService;
    
    @Inject
    private ConfigServiceBean configService;
    
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
    public CustomRule createCustomRule(CustomRule customRule, String featureName, String applicationName) throws RulesServiceException, RulesFaultException, AccessDeniedException, DaoException, RulesModelMarshallException, ModelMarshallException, MessageException {
        LOG.info("[INFO] Create invoked in service layer");

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
        CustomRule createdRule = null;
        //Copy-paste from RulesDomainModelBean to remove that class
        LOG.debug("Create in Rules");
        //CustomRule entity = CustomRuleMapper.toCustomRuleEntity(customRule);

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
        createdRule = customRule;

        // TODO: Rewrite so rules are loaded when changed
        rulesValidator.updateCustomRules();
        auditService.sendAuditMessage(AuditObjectTypeEnum.CUSTOM_RULE, AuditOperationEnum.CREATE, createdRule.getGuid(), null, customRule.getUpdatedBy());
        return createdRule;


    }

    /**
     * {@inheritDoc}
     *
     * @param guid
     * @return
     * @throws RulesServiceException
     */
    @Override
    public CustomRule getCustomRuleByGuid(String guid) throws RulesServiceException, RulesModelMapperException, RulesFaultException {
        LOG.info("[INFO] Get Custom Rule by guid invoked in service layer");
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
     * @throws RulesServiceException
     */
    @Override
    public CustomRule updateCustomRule(CustomRule oldCustomRule, String featureName, String applicationName) throws RulesServiceException, AccessDeniedException, RulesModelMarshallException, ModelMarshallException, RulesModelException, MessageException, DaoException {
        LOG.info("[INFO] Update custom rule invoked in service layer");
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
    //Copy from RulesDomainModelBean to remove that class
    private CustomRule internalUpdateCustomRule(CustomRule newEntity)  throws RulesModelException, DaoException {
        LOG.debug("Update custom rule in Rules");

        if (newEntity == null) {
            LOG.error("[ERROR] Custom Rule is null, returning Exception ]");
            throw new eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException("Custom Rule is null", null);
        }

        if (newEntity.getGuid() == null) {
            LOG.error("[ERROR] GUID of Custom Rule is null, returning Exception. ]");
            throw new eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException("GUID of Custom Rule is null", null);
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
        oldEntity.setEndDate(DateUtils.nowUTC().toGregorianCalendar().getTime());
        // Copy subscription list (ignore if provided)
        List<RuleSubscription> subscriptions = oldEntity.getRuleSubscriptionList();
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
     * @throws RulesServiceException
     */
    @Override
    public CustomRule updateCustomRule(CustomRule oldCustomRule) throws RulesServiceException, RulesModelException, DaoException {
        LOG.info("[INFO] Update custom rule invoked in service layer by timer");

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
    public CustomRule updateSubscription(UpdateSubscriptionType updateSubscriptionType, String username) throws RulesServiceException, RulesFaultException, eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException, DaoException, DaoMappingException {
        LOG.info("[INFO] Update subscription invoked in service layer");
        if (updateSubscriptionType == null) {
            LOG.error("[ERROR] Subscription is null, returning Exception ]");
            throw new eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException("Subscription is null", null);
        }

        boolean validRequest = updateSubscriptionType.getSubscription().getType() != null && updateSubscriptionType.getSubscription().getOwner() != null;
        if (!validRequest) {
            throw new RulesServiceException("Not a valid subscription!");
        }

        LOG.debug("Update custom rule subscription in Rules");


        if (updateSubscriptionType.getRuleGuid() == null) {
            LOG.error("[ERROR] Custom Rule GUID for Subscription is null, returning Exception. ]");
            throw new eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException("Custom Rule GUID for Subscription is null", null);
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
    public CustomRule deleteCustomRule(String guid, String username, String featureName, String applicationName) throws RulesServiceException, RulesFaultException, AccessDeniedException, DaoException, RulesModelMapperException {
        LOG.info("[INFO] Deleting custom rule by guid: {}.", guid);
        if (guid == null) {
            throw new InputArgumentException("No custom rule to remove");
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
        //CustomRuleType deletedRule = CustomRuleMapper.toCustomRuleType(entity);

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
    public GetAlarmListByQueryResponse getAlarmList(AlarmQuery query) throws RulesServiceException, RulesModelException, DaoMappingException, DaoException {
        LOG.info("[INFO] Get alarm list invoked in service layer");

        AlarmListResponseDto alarmList = getAlarmListByQuery(query);
        GetAlarmListByQueryResponse response = new GetAlarmListByQueryResponse();
        response.getAlarms().addAll(alarmList.getAlarmList());
        response.setTotalNumberOfPages(alarmList.getTotalNumberOfPages());
        response.setCurrentPage(alarmList.getCurrentPage());
        return response;

    }

    @Override
    public GetTicketListByQueryResponse getTicketList(String loggedInUser, TicketQuery query) throws RulesServiceException, SearchMapperException, DaoException, DaoMappingException, eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException {
        LOG.info("[INFO] Get ticket list invoked in service layer");

        if (query == null) {
            throw new eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException("Ticket list query is null");
        }
        if (query.getPagination() == null) {
            throw new eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException("Pagination in ticket list query is null");
        }

        Integer listSize = query.getPagination().getListSize();
        List<TicketSearchValue> searchKeyValues = TicketSearchFieldMapper.mapSearchField(query.getTicketSearchCriteria());
        List<String> validRuleGuids = rulesDao.getCustomRulesForTicketsByUser(loggedInUser);


        String sql = TicketSearchFieldMapper.createSelectSearchSql(searchKeyValues, validRuleGuids, true);
        String countSql = TicketSearchFieldMapper.createCountSearchSql(searchKeyValues, validRuleGuids, true);
        Long numberMatches = rulesDao.getTicketListSearchCount(countSql, searchKeyValues);
        List<Ticket> ticketEntityList = rulesDao.getTicketListPaginated(query.getPagination().getPage(), listSize, sql, searchKeyValues);

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
    public GetTicketListByMovementsResponse getTicketsByMovements(List<String> movements) throws RulesServiceException, eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException, DaoException, DaoMappingException {
        LOG.info("[INFO] Get tickets by movements invoked in service layer");
        if (movements == null) {
            throw new eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException("Movements list is null");
        }
        if (movements.isEmpty()) {
            throw new eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException("Movements list is empty");
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
    public GetTicketsAndRulesByMovementsResponse getTicketsAndRulesByMovements(List<String> movements) throws RulesServiceException {
        LOG.info("[INFO] Get tickets and rules by movements invoked in service layer");
        try {
            LOG.info("[INFO] Get Tickets and Rules by movements");

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
        } catch (DaoException | DaoMappingException e) {
            LOG.error("[ERROR] Error when getting list {}", e.getMessage());
            throw new RulesServiceException(e.getMessage());
        }
    }

    @Override
    public long countTicketsByMovements(List<String> movements) throws RulesServiceException, eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException, DaoException {
        LOG.info("[INFO] Get number of tickets by movements invoked in service layer");


        if (movements == null) {
            throw new eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException("Movements list is null");
        }
        if (movements.isEmpty()) {
            throw new eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException("Movements list is empty");
        }

        return rulesDao.countTicketListByMovements(movements);
    }

    @Override
    public Ticket updateTicketStatus(Ticket ticket) throws RulesServiceException, RulesFaultException, eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException, DaoException, DaoMappingException {
        LOG.info("[INFO] Update ticket status invoked in service layer");

        if (ticket == null || ticket.getGuid() == null) {
            LOG.error("[ERROR] Ticket is null, can not update status ]");
            throw new eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException("Ticket is null", null);
        }
        Ticket entity = rulesDao.getTicketByGuid(ticket.getGuid());

        entity.setStatus(ticket.getStatus());
        entity.setUpdated(new Date());
        entity.setUpdatedBy(ticket.getUpdatedBy());

        rulesDao.updateTicket(entity);

        //TicketType updatedTicket = TicketMapper.toTicketType(entity);

        // Notify long-polling clients of the update
        ticketUpdateEvent.fire(new NotificationMessage("guid", entity.getGuid()));
        // Notify long-polling clients of the change (no value since FE will need to fetch it)
        ticketCountEvent.fire(new NotificationMessage("ticketCount", null));
        auditService.sendAuditMessage(AuditObjectTypeEnum.TICKET, AuditOperationEnum.UPDATE, entity.getGuid(), /*ticket.getComment()*/"", ticket.getUpdatedBy());    //ticket.getComment was empty before the change
        return entity;

    }

    @Override
    public List<TicketType> updateTicketStatusByQuery(String loggedInUser, TicketQuery query, TicketStatusType status) throws RulesServiceException, eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException, DaoMappingException, DaoException, SearchMapperException {
        LOG.info("[INFO] Update all ticket status invoked in service layer");

        if (loggedInUser == null) {
            LOG.error("[ERROR] LoggedInUser is null, can not update status ]");
            throw new eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException("LoggedInUser is null", null);
        }
        if (status == null) {
            LOG.error("[ERROR] Status is null, can not update status ]");
            throw new eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException("Status is null", null);
        }
        if (query == null) {
            LOG.error("[ERROR] Status is null, can not update status ]");
            throw new eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException("Status is null", null);
        }
        List<TicketSearchValue> searchKeyValues = TicketSearchFieldMapper.mapSearchField(query.getTicketSearchCriteria());
        List<String> validRuleGuids = rulesDao.getCustomRulesForTicketsByUser(loggedInUser);
        String sql = TicketSearchFieldMapper.createSelectSearchSql(searchKeyValues, validRuleGuids, true);
        List<Ticket> tickets = rulesDao.getTicketList(sql, searchKeyValues);
        for (Ticket ticket : tickets) {
            ticket.setStatus(status.name());
            ticket.setUpdated(new Date());
            ticket.setUpdatedBy(loggedInUser);

            rulesDao.updateTicket(ticket);
        }
        List<TicketType> ticketList = new ArrayList<>();
        for (Ticket ticket : tickets) {
            ticketList.add(TicketMapper.toTicketType(ticket));

            // Notify long-polling clients of the update
            ticketUpdateEvent.fire(new NotificationMessage("guid", ticket.getGuid()));
            auditService.sendAuditMessage(AuditObjectTypeEnum.TICKET, AuditOperationEnum.UPDATE, ticket.getGuid(), null, loggedInUser);
        }


        // Notify long-polling clients of the change (no value since FE will need to fetch it)
        ticketCountEvent.fire(new NotificationMessage("ticketCount", null));
        return ticketList;

    }

    @Override
    public long getNumberOfAssetsNotSending() throws RulesServiceException {

        LOG.info("[INFO] Counting assets not sending");
        return rulesDao.getNumberOfTicketsByRuleGuid(UvmsConstants.ASSET_NOT_SENDING_RULE);

    }

    @Override
    public AlarmReport updateAlarmStatus(AlarmReport alarm) throws RulesServiceException, DaoException, eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException {
        LOG.info("[INFO] Update alarm status invoked in service layer");

        AlarmReport entity = rulesDao.getAlarmReportByGuid(alarm.getGuid());
        if (entity == null) {
            LOG.error("[ERROR] Alarm is null, can not update status ]");
            throw new eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException("Alarm is null", null);
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
    public List<PreviousReportType> getPreviousMovementReports() throws RulesServiceException, RulesFaultException {
        LOG.info("[INFO] Get previous movement reports invoked in service layer");
        try {
            LOG.info("[INFO] Getting list of previous reports");
            List<PreviousReportType> previousReports = new ArrayList<>();
            List<PreviousReport> entityList = rulesDao.getPreviousReportList();
            for (PreviousReport entity : entityList) {
                previousReports.add(PreviousReportMapper.toPreviousReportType(entity));
            }
            return previousReports;
        } catch (DaoException | DaoMappingException e) {
            LOG.error("[ERROR] Error when getting list {}", e.getMessage());
            throw new RulesServiceException("[ERROR] Error when getting list. ]", e);
        }
    }

    // Triggered by timer rule
    @Override
    public void timerRuleTriggered(String ruleName, PreviousReportFact fact) throws RulesServiceException, RulesFaultException {
        LOG.info("[INFO] Timer rule triggered invoked in service layer");
        try {
            // Check if ticket already is created for this asset
            LOG.info("[INFO] Getting ticket by asset guid : {}", fact.getAssetGuid());

            Ticket ticketEntity = rulesDao.getTicketByAssetAndRule(fact.getAssetGuid(), ruleName); //ruleName gets renamed into ruleGuid in the method, dont know what is correct
            TicketType ticket = TicketMapper.toTicketType(ticketEntity);

            if (ticket == null) {
                createAssetNotSendingTicket(ruleName, fact);
            } else if (ticket.getTicketCount() != null) {
                ticket.setTicketCount(ticket.getTicketCount() + 1);
                updateTicketCount(ticket);
            } else {
                ticket.setTicketCount(2L);
                updateTicketCount(ticket);
            }
        } catch (RulesModelException | DaoException | DaoMappingException e) {
            LOG.error("[ERROR] Error when getting list {}", e.getMessage());
            throw new RulesServiceException("[ERROR] Error when getting list", e);
        }
    }

    private void createAssetNotSendingTicket(String ruleName, PreviousReportFact fact) throws RulesModelException {
        TicketType ticketType = new TicketType();

        ticketType.setAssetGuid(fact.getAssetGuid());
        ticketType.setOpenDate(RulesUtil.dateToString(new Date()));
        ticketType.setRuleName(ruleName);
        ticketType.setRuleGuid(ruleName);
        ticketType.setUpdatedBy("UVMS");
        ticketType.setStatus(TicketStatusType.OPEN);
        ticketType.setMovementGuid(fact.getMovementGuid());
        ticketType.setGuid(UUID.randomUUID().toString());

        LOG.info("[INFO] Rule Engine creating Ticket");
        TicketType createdTicket = null;
        try {
            Ticket ticket = TicketMapper.toTicketEntity(ticketType);
            ticket.setTicketCount(1L);
            Ticket tempTicket = rulesDao.createTicket(ticket);
            createdTicket = TicketMapper.toTicketType(tempTicket);
        } catch (DaoException | DaoMappingException e) {
            LOG.error("[ERROR] Error when creating ticket {}", e.getMessage());
            throw new RulesModelException("[ERROR] Error when creating ticket. ]", e);
        }

		auditService.sendAuditMessage(AuditObjectTypeEnum.TICKET, AuditOperationEnum.CREATE, createdTicket.getGuid(), null, createdTicket.getUpdatedBy());	        // Notify long-polling clients of the change
        ticketCountEvent.fire(new NotificationMessage("ticketCount", null));
    }

    @Override
    public TicketType updateTicketCount(TicketType ticket) throws RulesServiceException {
        LOG.info("[INFO] Update ticket count invoked in service layer");
        try {
            LOG.info("[INFO] Update ticket count in Rules");

            if (ticket == null || ticket.getGuid() == null) {
                LOG.error("[ERROR] Ticket is null, can not update status ]");
                throw new eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException("Ticket is null", null);
            }
            Ticket entity = rulesDao.getTicketByGuid(ticket.getGuid());

            entity.setTicketCount(ticket.getTicketCount());
            entity.setUpdated(DateUtils.nowUTC().toGregorianCalendar().getTime());
            entity.setUpdatedBy(ticket.getUpdatedBy());

            rulesDao.updateTicket(entity);

            TicketType updatedTicket = TicketMapper.toTicketType(entity);

            //TicketType updatedTicket = rulesDomainModel.updateTicketCount(ticket);
            // Notify long-polling clients of the update
            ticketUpdateEvent.fire(new NotificationMessage("guid", updatedTicket.getGuid()));
            // Notify long-polling clients of the change (no value since FE will need to fetch it)
            ticketCountEvent.fire(new NotificationMessage("ticketCount", null));
            auditService.sendAuditMessage(AuditObjectTypeEnum.TICKET, AuditOperationEnum.UPDATE, updatedTicket.getGuid(), null, ticket.getUpdatedBy());
            return updatedTicket;
        } catch (RulesModelException | DaoMappingException e ) {
            LOG.error("[ERROR] Error when updating ticket status {}", e.getMessage());
            throw new RulesServiceException("[ERROR] Error when updating ticket status. ]", e);
        }
    }

    @Override
    public AlarmReport getAlarmReportByGuid(String guid) throws RulesServiceException, DaoException {
        LOG.info("[INFO] Getting alarm report by guid");
        return rulesDao.getAlarmReportByGuid(guid);
    }

    @Override
    public Ticket getTicketByGuid(String guid) throws RulesServiceException{

        LOG.info("[INFO] Getting ticket by guid");
        return rulesDao.getTicketByGuid(guid);
    }

    @Override
    public String reprocessAlarm(List<String> alarmGuids, String username) throws RulesServiceException, DaoMappingException, DaoException, RulesModelException {
        LOG.info("[INFO] Reprocess alarms invoked in service layer");

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
            setMovementReportReceived(rawMovementType, pluginType, username);
        }
        // return RulesDataSourceResponseMapper.mapToAlarmListFromResponse(response, messageId);
        // TODO: Better
        return "OK";

    }

    private AlarmListResponseDto getAlarmListByQuery(AlarmQuery query) throws RulesModelException, DaoMappingException, DaoException {
        LOG.info("[INFO] Get list of alarms from query.");

        if (query == null) {
            throw new eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException("Alarm list query is null");
        }
        if (query.getPagination() == null) {
            throw new eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException("Pagination in alarm list query is null");
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

    @Override
    public void setMovementReportReceived(final RawMovementType rawMovement, String pluginType, String username) throws RulesServiceException {
        try {
            Date auditTimestamp = new Date();
            Date auditTotalTimestamp = new Date();

            Asset asset = null;

            // Get Mobile Terminal if it exists
            MobileTerminalType mobileTerminal = mobileTerminalService.getMobileTerminalByRawMovement(rawMovement);
            auditTimestamp = auditLog("Time to fetch from Mobile Terminal Module:", auditTimestamp);

            // Get Asset
            if (mobileTerminal != null) {
                String connectId = mobileTerminal.getConnectId();
                if (connectId != null) {
                    asset = assetService.getAssetByConnectId(connectId);
                }
            } else {
                asset = assetService.getAssetByCfrIrcs(rawMovement.getAssetId());
                if (isPluginTypeWithoutMobileTerminal(rawMovement.getPluginType()) && asset != null) {
                    mobileTerminal = mobileTerminalService.findMobileTerminalByAsset(asset.getAssetId().getGuid());
                    rawMovement.setMobileTerminal(MobileTerminalMapper.mapMobileTerminal(mobileTerminal));
                }
            }
            if (rawMovement.getAssetId() == null && asset != null) {
                AssetId assetId = AssetAssetIdMapper.mapAssetToAssetId(asset);
                rawMovement.setAssetId(assetId);
            }
            auditTimestamp = auditLog("Time to fetch from Asset Module:", auditTimestamp);

            RawMovementFact rawMovementFact = RawMovementFactMapper.mapRawMovementFact(rawMovement, mobileTerminal, asset, pluginType);
            LOG.debug("rawMovementFact:{}", rawMovementFact);

            rulesValidator.evaluate(rawMovementFact);
            auditLog("Time to validate sanity:", auditTimestamp);

            if (rawMovementFact.isOk()) {
                MovementFact movementFact = collectMovementData(mobileTerminal, asset, rawMovement, username);
                LOG.info("[INFO] Validating movement from Movement Module");
                rulesValidator.evaluate(movementFact);
                auditLog("Rules total time:", auditTotalTimestamp);
                // Tell Exchange that a movement was persisted in Movement
                exchangeService.sendBackToExchange(movementFact.getMovementGuid(), rawMovement, MovementRefTypeType.MOVEMENT, username);
            } else {
                // Tell Exchange that the report caused an alarm
                exchangeService.sendBackToExchange(null, rawMovement, MovementRefTypeType.ALARM, username);
            }
        } catch (MessageException | MobileTerminalModelMapperException | MobileTerminalUnmarshallException | JMSException | AssetModelMapperException | RulesModelMapperException | InterruptedException | ExecutionException e) {
            throw new RulesServiceException(e.getMessage());
        }
    }

    private boolean isPluginTypeWithoutMobileTerminal(String pluginType) {
        if (pluginType == null) {
            return true;
        }
        try {
            PluginType type = PluginType.valueOf(pluginType);
            switch (type) {
                case MANUAL:
                case NAF:
                case OTHER:
                    return true;
                default:
                    return false;
            }
        } catch (IllegalArgumentException e) {
            return false;
        }
    }

    private MovementFact collectMovementData(MobileTerminalType mobileTerminal, Asset asset, final RawMovementType rawMovement, final String username) throws MessageException, RulesModelMapperException, ExecutionException, InterruptedException, RulesServiceException {
        int threadNum = 5;
        ExecutorService executor = Executors.newFixedThreadPool(threadNum);
        Integer numberOfReportsLast24Hours;
        final String assetGuid;
        final String assetHistGuid;
        final String assetFlagState;
        if (asset != null && asset.getAssetId() != null && asset.getEventHistory() != null) {
            assetGuid = asset.getAssetId().getGuid();
            assetHistGuid = asset.getEventHistory().getEventId();
            assetFlagState = asset.getCountryCode();
        } else {
            LOG.warn("[WARN] Asset was null for {} ", rawMovement.getAssetId());
            assetGuid = null;
            assetHistGuid = null;
            assetFlagState = null;
        }

        final Date positionTime = rawMovement.getPositionTime();

        FutureTask<Long> timeDiffAndPersistMovementTask = new FutureTask<>(new Callable<Long>() {
            @Override
            public Long call() {
                return timeDiffAndPersistMovement(rawMovement.getSource(), assetGuid, assetFlagState, positionTime);
            }
        });
        executor.execute(timeDiffAndPersistMovementTask);

        FutureTask<Integer> numberOfReportsLast24HoursTask = new FutureTask<>(new Callable<Integer>() {
            @Override
            public Integer call() {
                return movementService.numberOfReportsLast24Hours(assetHistGuid, positionTime);
            }
        });
        executor.execute(numberOfReportsLast24HoursTask);

        FutureTask<MovementType> sendToMovementTask = new FutureTask<>(new Callable<MovementType>() {
            @Override
            public MovementType call() {
                return movementService.sendToMovement(assetHistGuid, rawMovement, username);
            }
        });
        executor.execute(sendToMovementTask);

        FutureTask<List<AssetGroup>> assetGroupTask = new FutureTask<>(new Callable<List<AssetGroup>>() {
            @Override
            public List<AssetGroup> call() {
                return assetService.getAssetGroup(assetGuid);
            }
        });
        executor.execute(assetGroupTask);

        FutureTask<List<String>> vicinityOfTask = new FutureTask<>(new Callable<List<String>>() {
            @Override
            public List<String> call() {
                return movementService.getVicinityOf(rawMovement);
            }
        });
        executor.execute(vicinityOfTask);

        // Get channel guid
        String channelGuid = "";
        if (mobileTerminal != null) {
            channelGuid = getChannelGuid(mobileTerminal, rawMovement);
        }

        // Get channel type
        String comChannelType = null;
        if (rawMovement.getComChannelType() != null) {
            comChannelType = rawMovement.getComChannelType().name();
        }

        // Get data from parallel tasks
        try {
            Date auditParallelTimestamp = new Date();
            Long timeDiffInSeconds = timeDiffAndPersistMovementTask.get();
            List<AssetGroup> assetGroups = assetGroupTask.get();
            numberOfReportsLast24Hours = numberOfReportsLast24HoursTask.get();
            MovementType createdMovement = sendToMovementTask.get();
            List<String> vicinityOf = vicinityOfTask.get();
            auditLog("Total time for parallel tasks:", auditParallelTimestamp);

            MovementFact movementFact = MovementFactMapper.mapMovementFact(createdMovement, mobileTerminal, asset, comChannelType, assetGroups, timeDiffInSeconds, numberOfReportsLast24Hours, channelGuid, vicinityOf);
            LOG.debug("movementFact:{}", movementFact);

            executor.shutdown();
            return movementFact;
        } catch (RulesServiceException | NullPointerException e) {
            executor.shutdown();
            throw new RulesServiceException("Error likely caused by a duplicate movement.", e);
        }
    }

    private Long timeDiffAndPersistMovement(MovementSourceType movementSource, String assetGuid, String assetFlagState, Date positionTime) {
        Date auditTimestamp = new Date();

        // This needs to be done before persisting last report
        Long timeDiffInSeconds = null;
        Long timeDiff = timeDiffFromLastCommunication(assetGuid, positionTime);
        timeDiffInSeconds = timeDiff != null ? timeDiff / 1000 : null;
        auditTimestamp = auditLog("Time to fetch time difference to previous report:", auditTimestamp);

        // We only persist our own last communications that were not from AIS.
        if (configService.isLocalFlagstate(assetFlagState) && !movementSource.equals(MovementSourceType.AIS)) {
            persistLastCommunication(assetGuid, positionTime);
        }
        auditLog("Time to persist the position time:", auditTimestamp);

        return timeDiffInSeconds;
    }

    private Long timeDiffFromLastCommunication(String assetGuid, Date thisTime) {
        LOG.info("[INFO] Fetching time difference to previous movement report");
        Long timeDiff = null;
        try {
            LOG.info("[INFO] Getting previous report by asset GUID..");
            PreviousReport entity = rulesDao.getPreviousReportByAssetGuid(assetGuid);
            PreviousReportType previousReport = PreviousReportMapper.toPreviousReportType(entity);

            Date previousTime = previousReport.getPositionTime();
            timeDiff = thisTime.getTime() - previousTime.getTime();
        } catch (Exception e) { // there should be a DaoMappingException here but based on this exception and the below comment I am putting it in a comment instead.....
            // If something goes wrong, continue with the other validation
            LOG.error("[ERROR] Error when getting previous report by asset guid {}", e.getMessage());
            LOG.warn("[WARN] Error when fetching time difference of previous movement reports..");
        }
        return timeDiff;
    }

    private void persistLastCommunication(String assetGuid, Date positionTime) {
        PreviousReportType thisReport = new PreviousReportType();
        thisReport.setPositionTime(positionTime);
        thisReport.setAssetGuid(assetGuid);
        try {
            LOG.info("[INFO] Upserting previous report");

            PreviousReport entity = rulesDao.getPreviousReportByAssetGuid(thisReport.getAssetGuid());
            if (entity == null) {
                entity = PreviousReportMapper.toPreviousReportEntity(thisReport);
            } else {
                entity = PreviousReportMapper.toPreviousReportEntity(entity, thisReport);
            }
            rulesDao.updatePreviousReport(entity);

        } catch (DaoException | DaoMappingException e) {
            LOG.error("[ERROR] Error persisting report. ] {}", e.getMessage());
        }
    }

    // TODO: Implement for IRIDIUM as well (if needed)
    private String getChannelGuid(MobileTerminalType mobileTerminal, RawMovementType rawMovement) {
        String dnid = "";
        String memberNumber = "";
        String channelGuid = "";

        List<IdList> ids = rawMovement.getMobileTerminal().getMobileTerminalIdList();

        for (IdList id : ids) {
            switch (id.getType()) {
                case DNID:
                    if (id.getValue() != null) {
                        dnid = id.getValue();
                    }
                    break;
                case MEMBER_NUMBER:
                    if (id.getValue() != null) {
                        memberNumber = id.getValue();
                    }
                    break;
                case SERIAL_NUMBER:
                    // IRIDIUM
                case LES:
                default:
                    LOG.error("[ERROR] Unhandled Mobile Terminal id: {} ]", id.getType());
                    break;
            }
        }

        // Get the channel guid
        boolean correctDnid = false;
        boolean correctMemberNumber = false;
        List<ComChannelType> channels = mobileTerminal.getChannels();
        for (ComChannelType channel : channels) {

            List<ComChannelAttribute> attributes = channel.getAttributes();

            for (ComChannelAttribute attribute : attributes) {
                String type = attribute.getType();
                String value = attribute.getValue();

                if ("DNID".equals(type)) {
                    correctDnid = value.equals(dnid);
                }
                if ("MEMBER_NUMBER".equals(type)) {
                    correctMemberNumber = value.equals(memberNumber);
                }
            }

            if (correctDnid && correctMemberNumber) {
                channelGuid = channel.getGuid();
            }
        }

        return channelGuid;
    }

    private Date auditLog(String msg, Date lastTimestamp) {
        Date newTimestamp = new Date();
        long duration = newTimestamp.getTime() - lastTimestamp.getTime();
        LOG.debug("[INFO] --> AUDIT - {} {} ms", msg, duration);
        return newTimestamp;
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
