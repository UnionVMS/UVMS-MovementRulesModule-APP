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

import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.*;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketsAndRulesByMovementsResponse;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.CustomRuleQuery;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.TicketQuery;
import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketStatusType;
import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketType;
import eu.europa.ec.fisheries.schema.movementrules.ticketrule.v1.TicketAndRuleType;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageException;
import eu.europa.ec.fisheries.uvms.commons.notifications.NotificationMessage;
import eu.europa.ec.fisheries.uvms.movementrules.service.boundary.AuditServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.boundary.UserServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.CustomRuleValidator;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.RulesValidator;
import eu.europa.ec.fisheries.uvms.movementrules.service.constants.AuditObjectTypeEnum;
import eu.europa.ec.fisheries.uvms.movementrules.service.constants.AuditOperationEnum;
import eu.europa.ec.fisheries.uvms.movementrules.service.constants.ServiceConstants;
import eu.europa.ec.fisheries.uvms.movementrules.service.dao.RulesDao;
import eu.europa.ec.fisheries.uvms.movementrules.service.dto.CustomRuleListResponseDto;
import eu.europa.ec.fisheries.uvms.movementrules.service.dto.TicketListResponseDto;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.PreviousReport;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.RuleSubscription;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Ticket;
import eu.europa.ec.fisheries.uvms.movementrules.service.event.TicketCountEvent;
import eu.europa.ec.fisheries.uvms.movementrules.service.event.TicketEvent;
import eu.europa.ec.fisheries.uvms.movementrules.service.event.TicketUpdateEvent;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.CustomRuleMapper;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.TicketMapper;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.search.CustomRuleSearchFieldMapper;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.search.CustomRuleSearchValue;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.search.TicketSearchFieldMapper;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.search.TicketSearchValue;
import eu.europa.ec.fisheries.uvms.user.model.exception.ModelMarshallException;
import eu.europa.ec.fisheries.wsdl.user.types.Feature;
import eu.europa.ec.fisheries.wsdl.user.types.UserContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.ejb.Stateless;
import javax.enterprise.event.Event;
import javax.inject.Inject;
import javax.persistence.NoResultException;
import javax.servlet.ServletContext;
import java.nio.file.AccessDeniedException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Stateless
public class RulesServiceBean {

    private static final Logger LOG = LoggerFactory.getLogger(RulesServiceBean.class);

    @Inject
    private RulesValidator rulesValidator;

    @Inject
    private UserServiceBean userService;
    
    @Inject
    private RulesDao rulesDao;

    @Inject
    private AuditServiceBean auditService;

    @Inject
    @TicketEvent
    private Event<NotificationMessage> ticketEvent;

    @Inject
    @TicketUpdateEvent
    private Event<NotificationMessage> ticketUpdateEvent;

    @Inject
    @TicketCountEvent
    private Event<NotificationMessage> ticketCountEvent;

    public CustomRule createCustomRule(CustomRule customRule, String featureName, String applicationName) throws AccessDeniedException, ModelMarshallException, MessageException {
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


        customRule.setUpdated(Instant.now());
        customRule.setStartDate(Instant.now());


        rulesDao.createCustomRule(customRule);

        // TODO: Rewrite so rules are loaded when changed
        rulesValidator.updateCustomRules();
        auditService.sendAuditMessage(AuditObjectTypeEnum.CUSTOM_RULE, AuditOperationEnum.CREATE, customRule.getGuid().toString(), null, customRule.getUpdatedBy());
        return customRule;


    }

    public CustomRule getCustomRuleByGuid(UUID guid) {
        try {
            CustomRule retVal = rulesDao.getCustomRuleByGuid(guid);
            retVal.setLastTriggered(getLastTriggeredForRule(guid));
            return retVal;
        } catch (NoResultException e) {
            LOG.error("[ERROR] Error when getting CustomRule by GUID ] {}", e.getMessage());
            throw new RuntimeException(e);
        }
    }
    
    public List<CustomRule> getCustomRulesByUser(String userName) {
        List<CustomRule> customRules = rulesDao.getCustomRulesByUser(userName);
        for (CustomRule customRule: customRules) {         //this might not be the fastest solution but as long as the number of results are small it should be fine with an extra DB query per result
            customRule.setLastTriggered(getLastTriggeredForRule(customRule.getGuid()));
        }
        return customRules;
    }
    
    public List<CustomRule> getRunnableCustomRules() {
        return rulesDao.getRunnableCustomRuleList();
    }

    public CustomRuleListResponseDto getCustomRulesByQuery(CustomRuleQuery query) {
        if (query == null) {
            throw new IllegalArgumentException("Custom rule list query is null");
        }
        if (query.getPagination() == null) {
            throw new IllegalArgumentException("Pagination in custom rule list query is null");
        }

        Integer page = query.getPagination().getPage();
        Integer listSize = query.getPagination().getListSize();

        List<CustomRuleSearchValue> searchKeyValues = CustomRuleSearchFieldMapper.mapSearchField(query.getCustomRuleSearchCriteria());

        String sql = CustomRuleSearchFieldMapper.createSelectSearchSql(searchKeyValues, query.isDynamic());
        String countSql = CustomRuleSearchFieldMapper.createCountSearchSql(searchKeyValues, query.isDynamic());

        Long numberMatches = rulesDao.getCustomRuleListSearchCount(countSql);
        List<CustomRule> customRuleEntityList = rulesDao.getCustomRuleListPaginated(page, listSize, sql);

        for (CustomRule customRule: customRuleEntityList) {         //this might not be the fastest solution but as long as the number of results are small it should be fine with an extra DB query per result
                customRule.setLastTriggered(getLastTriggeredForRule(customRule.getGuid()));
        }

        
        int numberOfPages = (int) (numberMatches / listSize);
        if (numberMatches % listSize != 0) {
            numberOfPages += 1;
        }

        CustomRuleListResponseDto customRuleListByQuery = new CustomRuleListResponseDto();
        customRuleListByQuery.setTotalNumberOfPages(numberOfPages);
        customRuleListByQuery.setCurrentPage(query.getPagination().getPage());
        customRuleListByQuery.setCustomRuleList(customRuleEntityList);
        return customRuleListByQuery;
    }

    public CustomRule updateCustomRule(CustomRule oldCustomRule, String featureName, String applicationName) throws ModelMarshallException, MessageException, AccessDeniedException {
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
        auditService.sendAuditMessage(AuditObjectTypeEnum.CUSTOM_RULE, AuditOperationEnum.UPDATE, customRule.getGuid().toString(), null, oldCustomRule.getUpdatedBy());
        return customRule;

    }

    private CustomRule internalUpdateCustomRule(CustomRule newEntity) {
        if (newEntity == null) {
            throw new IllegalArgumentException("Custom Rule is null");
        }

        if (newEntity.getGuid() == null) {
            throw new IllegalArgumentException("GUID of Custom Rule is null");
        }

        CustomRule oldEntity = getCustomRuleByGuid(newEntity.getGuid());

        CustomRule copiedNewEntity = newEntity.copy();


        // Close old version
        oldEntity.setArchived(true);
        oldEntity.setActive(false);
        oldEntity.setEndDate(Instant.now());
        // Copy subscription list (ignore if provided)
        // Copy to new array to avoid concurrent modification exception
        List<RuleSubscription> subscriptions = new ArrayList<>(oldEntity.getRuleSubscriptionList());
        for (RuleSubscription subscription : subscriptions) {
            rulesDao.detachSubscription(subscription);
            copiedNewEntity.getRuleSubscriptionList().add(subscription);
            subscription.setCustomRule(copiedNewEntity);
        }

        copiedNewEntity.setUpdated(Instant.now());
        copiedNewEntity.setStartDate(Instant.now());
        copiedNewEntity.setGuid(null);
        copiedNewEntity = rulesDao.createCustomRule(copiedNewEntity);
        return copiedNewEntity;
    }

    public CustomRule updateCustomRule(CustomRule oldCustomRule) {
        CustomRule updatedCustomRule = internalUpdateCustomRule(oldCustomRule);
        auditService.sendAuditMessage(AuditObjectTypeEnum.CUSTOM_RULE, AuditOperationEnum.UPDATE, updatedCustomRule.getGuid().toString(), null, oldCustomRule.getUpdatedBy());
        return updatedCustomRule;

    }

    public CustomRule updateSubscription(UpdateSubscriptionType updateSubscriptionType, String username) {
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

        CustomRule customRuleEntity = rulesDao.getCustomRuleByGuid(UUID.fromString(updateSubscriptionType.getRuleGuid()));

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

    public CustomRule deleteCustomRule(String guidString, String username, String featureName, String applicationName) throws AccessDeniedException {
        LOG.info("[INFO] Deleting custom rule by guid: {}.", guidString);
        if (guidString == null) {
            throw new IllegalArgumentException("No custom rule to remove");
        }

        UUID guid = UUID.fromString(guidString);
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
        entity.setEndDate(Instant.now());

        rulesValidator.updateCustomRules();
        auditService.sendAuditMessage(AuditObjectTypeEnum.CUSTOM_RULE, AuditOperationEnum.DELETE, entity.getGuid().toString(), null, username);
        return entity;

    }

    public TicketListResponseDto getTicketList(String loggedInUser, TicketQuery query) {
        if (query == null) {
            throw new IllegalArgumentException("Ticket list query is null");
        }
        if (query.getPagination() == null) {
            throw new IllegalArgumentException("Pagination in ticket list query is null");
        }

        Integer listSize = query.getPagination().getListSize();
        List<TicketSearchValue> searchKeyValues = TicketSearchFieldMapper.mapSearchField(query.getTicketSearchCriteria());
        List<UUID> validRuleGuids = rulesDao.getCustomRulesForTicketsByUser(loggedInUser);
        List<String> validRuleStrings = new ArrayList<>();
        for (UUID uuid: validRuleGuids) {
            validRuleStrings.add(uuid.toString());
        }

        String sql = TicketSearchFieldMapper.createSelectSearchSql(searchKeyValues, validRuleStrings, true);
        String countSql = TicketSearchFieldMapper.createCountSearchSql(searchKeyValues, validRuleStrings, true);
        Long numberMatches = rulesDao.getTicketListSearchCount(countSql);
        List<Ticket> ticketEntityList = rulesDao.getTicketListPaginated(query.getPagination().getPage(), listSize, sql);

        int numberOfPages = (int) (numberMatches / listSize);
        if (numberMatches % listSize != 0) {
            numberOfPages += 1;
        }

        TicketListResponseDto ticketListDto = new TicketListResponseDto();
        ticketListDto.setCurrentPage(query.getPagination().getPage());
        ticketListDto.setTotalNumberOfPages(numberOfPages);
        ticketListDto.setTicketList(ticketEntityList);
        return ticketListDto;
    }

    public Instant getLastTriggeredForRule(UUID ruleGuid){
        Ticket ticket = rulesDao.getLatestTicketForRule(ruleGuid);
        Instant retVal = null;
        if(ticket != null){
            retVal = ticket.getCreatedDate();
        }
        return retVal;
    }

    public List<Ticket> getTicketsByMovements(List<String> movements) {
        if (movements == null) {
            throw new IllegalArgumentException("Movements list is null");
        }
        if (movements.isEmpty()) {
            throw new IllegalArgumentException("Movements list is empty");
        }

        return rulesDao.getTicketsByMovements(movements);
    }

    public GetTicketsAndRulesByMovementsResponse getTicketsAndRulesByMovements(List<String> movementGuidList) {
        List<TicketAndRuleType> ticketsAndRules = new ArrayList<>();
        // TODO: This can be done more efficiently with some join stuff
        List<Ticket> tickets = rulesDao.getTicketsByMovements(movementGuidList);
        for (Ticket ticket : tickets) {
            CustomRule rule = rulesDao.getCustomRuleByGuid(UUID.fromString(ticket.getRuleGuid()));
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

    public long countTicketsByMovements(List<String> movements) {
        if (movements == null) {
            throw new IllegalArgumentException("Movements list is null");
        }
        if (movements.isEmpty()) {
            throw new IllegalArgumentException("Movements list is empty");
        }

        return rulesDao.countTicketListByMovements(movements);
    }

    public Ticket updateTicketStatus(Ticket ticket) {
        if (ticket == null || ticket.getGuid() == null) {
            throw new IllegalArgumentException("Ticket is null");
        }
        Ticket entity = rulesDao.getTicketByGuid(ticket.getGuid());

        entity.setStatus(ticket.getStatus());
        entity.setUpdated(Instant.now());
        entity.setUpdatedBy(ticket.getUpdatedBy());

        rulesDao.updateTicket(entity);

        // Notify long-polling clients of the update
        ticketUpdateEvent.fire(new NotificationMessage("guid", entity.getGuid()));
        // Notify long-polling clients of the change (no value since FE will need to fetch it)
        ticketCountEvent.fire(new NotificationMessage("ticketCount", null));
        auditService.sendAuditMessage(AuditObjectTypeEnum.TICKET, AuditOperationEnum.UPDATE, entity.getGuid().toString(), "", ticket.getUpdatedBy());
        return entity;

    }

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
        List<UUID> validRuleGuids = rulesDao.getCustomRulesForTicketsByUser(loggedInUser);
        List<String> validRuleStrings = new ArrayList<>();
        for (UUID uuid: validRuleGuids) {
            validRuleStrings.add(uuid.toString());
        }
        String sql = TicketSearchFieldMapper.createSelectSearchSql(searchKeyValues, validRuleStrings, true);
        List<Ticket> tickets = rulesDao.getTicketList(sql);
        for (Ticket ticket : tickets) {
            ticket.setStatus(status.name());
            ticket.setUpdated(Instant.now());
            ticket.setUpdatedBy(loggedInUser);

            rulesDao.updateTicket(ticket);

            // Notify long-polling clients of the update
            ticketUpdateEvent.fire(new NotificationMessage("guid", ticket.getGuid()));
            auditService.sendAuditMessage(AuditObjectTypeEnum.TICKET, AuditOperationEnum.UPDATE, ticket.getGuid().toString(), null, loggedInUser);
        }

        // Notify long-polling clients of the change (no value since FE will need to fetch it)
        ticketCountEvent.fire(new NotificationMessage("ticketCount", null));
        return tickets;

    }

    public long getNumberOfAssetsNotSending() {
        return rulesDao.getNumberOfTicketsWithAssetNotSending(ServiceConstants.ASSET_NOT_SENDING_RULE);
    }


    // Triggered by RulesTimerBean
    public List<PreviousReport> getPreviousMovementReports() {
        return rulesDao.getPreviousReportList();
    }

    // Triggered by timer rule
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
        ticket.setCreatedDate(Instant.now());
        ticket.setRuleName(ruleName);
        ticket.setRuleGuid(ruleName);
        ticket.setUpdatedBy("UVMS");
        ticket.setUpdated(Instant.now());
        ticket.setStatus(TicketStatusType.OPEN.value());
        ticket.setTicketCount(1L);
        rulesDao.createTicket(ticket);

		auditService.sendAuditMessage(AuditObjectTypeEnum.TICKET, AuditOperationEnum.CREATE, ticket.getGuid().toString(), null, ticket.getUpdatedBy());
		// Notify long-polling clients of the change
        ticketCountEvent.fire(new NotificationMessage("ticketCount", null));
    }

    public Ticket updateTicketCount(Ticket ticket) {
        if (ticket == null || ticket.getGuid() == null) {
            throw new IllegalArgumentException("Ticket is null, can not upate status");
        }
        ticket.setUpdated(Instant.now());

        // Notify long-polling clients of the update
        ticketUpdateEvent.fire(new NotificationMessage("guid", ticket.getGuid()));
        // Notify long-polling clients of the change (no value since FE will need to fetch it)
        ticketCountEvent.fire(new NotificationMessage("ticketCount", null));
        auditService.sendAuditMessage(AuditObjectTypeEnum.TICKET, AuditOperationEnum.UPDATE, ticket.getGuid().toString(), null, ticket.getUpdatedBy());
        return ticket;
    }


    public Ticket getTicketByGuid(UUID guid){
        return rulesDao.getTicketByGuid(guid);
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

    public boolean isValid(CustomRuleType customRule) {
        return CustomRuleValidator.isCustomRuleValid(customRule);
    }

    public String getApplicationName(ServletContext servletContext) {
        String cfgName = servletContext.getInitParameter("usmApplication");
        if (cfgName == null) {
            cfgName = "Union-VMS";
        }
        return cfgName;
    }
}
