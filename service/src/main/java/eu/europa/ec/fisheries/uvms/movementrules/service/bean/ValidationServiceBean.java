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

import eu.europa.ec.fisheries.schema.exchange.movement.v1.MovementType;
import eu.europa.ec.fisheries.schema.exchange.movement.v1.MovementTypeType;
import eu.europa.ec.fisheries.schema.exchange.plugin.types.v1.EmailType;
import eu.europa.ec.fisheries.schema.exchange.plugin.types.v1.PluginType;
import eu.europa.ec.fisheries.schema.exchange.service.v1.ServiceResponseType;
import eu.europa.ec.fisheries.schema.exchange.service.v1.StatusType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.ActionType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.SubscriptionTypeType;
import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketStatusType;
import eu.europa.ec.fisheries.uvms.commons.date.JsonBConfigurator;
import eu.europa.ec.fisheries.uvms.commons.notifications.NotificationMessage;
import eu.europa.ec.fisheries.uvms.mobileterminal.model.dto.CreatePollResultDto;
import eu.europa.ec.fisheries.uvms.movementrules.model.dto.MovementDetails;
import eu.europa.ec.fisheries.uvms.movementrules.service.boundary.AuditServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.boundary.ExchangeServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.boundary.UserServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.constants.AuditObjectTypeEnum;
import eu.europa.ec.fisheries.uvms.movementrules.service.constants.AuditOperationEnum;
import eu.europa.ec.fisheries.uvms.movementrules.service.dao.RulesDao;
import eu.europa.ec.fisheries.uvms.movementrules.service.dto.EventTicket;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.RuleSubscription;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Ticket;
import eu.europa.ec.fisheries.uvms.movementrules.service.event.TicketCountEvent;
import eu.europa.ec.fisheries.uvms.movementrules.service.event.TicketEvent;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.EmailMapper;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.ExchangeMovementMapper;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.bean.IncidentProducer;
import eu.europa.ec.fisheries.uvms.rest.security.InternalRestTokenHandler;
import eu.europa.ec.fisheries.wsdl.user.module.GetContactDetailResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.Resource;
import javax.ejb.EJB;
import javax.ejb.Stateless;
import javax.enterprise.event.Event;
import javax.inject.Inject;
import javax.jms.JMSException;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Stateless
public class ValidationServiceBean  {

    private static final Logger LOG = LoggerFactory.getLogger(ValidationServiceBean.class);

    @EJB
    private RulesDao rulesDao;
    
    @Inject
    private UserServiceBean userService;
    
    @Inject
    private ExchangeServiceBean exchangeService;
    
    @Inject
    private AuditServiceBean auditService;

    @Inject
    private IncidentProducer incidentProducer;

    @Inject
    @TicketEvent
    private Event<EventTicket> ticketEvent;

    @Inject
    @TicketCountEvent
    private Event<NotificationMessage> ticketCountEvent;

    @EJB
    private InternalRestTokenHandler tokenHandler;

    // Triggered by rule engine
    public void customRuleTriggered(String ruleName, String ruleGuid, MovementDetails movementDetails, String actions) {
        LOG.debug("Performing actions on triggered user rules, rule: {}", ruleName);

        CustomRule triggeredRule = getCustomRule(ruleGuid);
        triggeredRule.setLastTriggered(Instant.now());
        
        Instant auditTimestamp = Instant.now();

        auditTimestamp = auditLog("Time to create/update ticket:", auditTimestamp);

        sendMailToSubscribers(triggeredRule, movementDetails);
        auditTimestamp = auditLog("Time to send email to subscribers:", auditTimestamp);

        // Actions list format:
        // ACTION,TARGET,VALUE;ACTION,TARGET,VALUE;
        // N.B! The .drl rule file gives the string "null" when (for instance)
        // value is null.
        String[] parsedActionKeyValueList = actions.split(";");
        for (String keyValue : parsedActionKeyValueList) {
            String[] keyValueList = keyValue.split(",");
            String action = keyValueList[0];
            String target = "";
            String value = "";
            if (keyValueList.length == 3) {
                target = keyValueList[1];
                value = keyValueList[2];
            }
            switch (ActionType.valueOf(action)) {
                case EMAIL:
                    // Value=address.
                    sendToEmail(value, ruleName, movementDetails);
                    auditTimestamp = auditLog("Time to send (action) email:", auditTimestamp);
                    break;
                case SEND_REPORT:
                    sendToEndpoint(ruleName, movementDetails, value, target, ActionType.SEND_REPORT);
                    auditTimestamp = auditLog("Time to send to endpoint:", auditTimestamp);
                    break;
                case SEND_ENTRY_REPORT:
                    sendToEndpoint(ruleName, movementDetails, value, target, ActionType.SEND_ENTRY_REPORT);
                    break;
                case SEND_EXIT_REPORT:
                    sendToEndpoint(ruleName, movementDetails, value, target, ActionType.SEND_EXIT_REPORT);
                    break;
                case MANUAL_POLL:
                    createPollInternal(movementDetails, ruleName);
                    auditTimestamp = auditLog("Time to send poll:", auditTimestamp);
                    break;
                case CREATE_INCIDENT:
                    Ticket ticket = upsertTicket(triggeredRule, movementDetails);
                    incidentProducer.createdTicket(new EventTicket(ticket, triggeredRule));
                    break;
                case CREATE_TICKET:
                    upsertTicket(triggeredRule, movementDetails);
                    break;

                    /*
                case ON_HOLD:
                    LOG.info("NOT IMPLEMENTED!");
                    break;
                case TOP_BAR_NOTIFICATION:
                    LOG.info("NOT IMPLEMENTED!");
                    break;
                case SMS:
                    LOG.info("NOT IMPLEMENTED!");
                    break;
                    */
                default:
                    LOG.info("The action '{}' is not defined", action);
                    break;
            }
        }
    }

    private Ticket upsertTicket(CustomRule triggeredRule, MovementDetails movementDetails){
        if (triggeredRule != null && triggeredRule.isAggregateInvocations()) {
            createTicketOrIncreaseCount(movementDetails, triggeredRule);
        } else {
            createTicket(triggeredRule, movementDetails);
        }
    }


    private CustomRule getCustomRule(String ruleGuid) {
        try {
            return rulesDao.getCustomRuleByGuid(UUID.fromString(ruleGuid));
        } catch (Exception e) {
            LOG.error("[ Failed to fetch rule when sending email to subscribers due to erro when getting CustomRule by GUID! ] {}", e.getMessage());
            return null;
        }
    }

    private void createTicketOrIncreaseCount(MovementDetails movementDetails, CustomRule triggeredRule) {
        Ticket latestTicketForRule = rulesDao.getLatestTicketForRule(triggeredRule.getGuid());
        if (latestTicketForRule == null) {
            createTicket(triggeredRule, movementDetails);
        } else {
            latestTicketForRule.setTicketCount(latestTicketForRule.getTicketCount() + 1);
            latestTicketForRule.setUpdated(Instant.now());
        }
    }
    
    private void sendMailToSubscribers(CustomRule customRule, MovementDetails movementDetails) {
        if (customRule == null) {
            return;
        }
        
        List<RuleSubscription> subscriptions = customRule.getRuleSubscriptionList();
        if (subscriptions != null) {
            for (RuleSubscription subscription : subscriptions) {
                if (SubscriptionTypeType.EMAIL.value().equals(subscription.getType())) {
                    try {
                        // Find current email address
                        GetContactDetailResponse userResponse = userService.getContactDetails(subscription.getOwner());
                        String emailAddress = userResponse.getContactDetails().getEMail();
                        sendToEmail(emailAddress, customRule.getName(), movementDetails);
                    } catch (Exception e) {
                        // If a mail attempt fails, proceed with the rest
                        LOG.error("Could not send email to user '{}'", subscription.getOwner());
                    }
                }
            }
        }
    }

    private void sendToEndpoint(String ruleName, MovementDetails movementDetails, String organisationName, String pluginName, ActionType actionType) {
        LOG.debug("Sending to organisation '{}'", organisationName);

        try {
            MovementType exchangeMovement = ExchangeMovementMapper.mapToExchangeMovementType(movementDetails);

            mapTypeOfMessage(exchangeMovement, actionType);

            exchangeService.sendReportToPlugin(pluginName, ruleName, organisationName, exchangeMovement, new ArrayList<>(), movementDetails);

            auditService.sendAuditMessage(AuditObjectTypeEnum.CUSTOM_RULE_ACTION, AuditOperationEnum.SEND_TO_ENDPOINT, null, organisationName, "UVMS");

        } catch (Exception e) {
            LOG.error("[ Failed to send to endpoint! ] {}", e.getMessage());
        }
    }

    private void mapTypeOfMessage(MovementType movement, ActionType actionType) {
        if (actionType == null) {
            return;
        }
        if (ActionType.SEND_ENTRY_REPORT.equals(actionType)) {
            movement.setMovementType(MovementTypeType.ENT);
        } else if (ActionType.SEND_EXIT_REPORT.equals(actionType) ) {
            movement.setMovementType(MovementTypeType.EXI);
        }
    }
    
    private void sendToEmail(String emailAddress, String ruleName, MovementDetails movementDetails) {
        LOG.info("Sending email to '{}'", emailAddress);

        EmailType email = new EmailType();

        email.setSubject(EmailMapper.buildSubject(movementDetails));
        email.setBody(EmailMapper.buildBody(ruleName, movementDetails));
        email.setTo(emailAddress);

        try {
            List<ServiceResponseType> pluginList = exchangeService.getPluginList(PluginType.EMAIL);
            if (pluginList != null && !pluginList.isEmpty()) {
                for (ServiceResponseType service : pluginList) {
                    if (StatusType.STOPPED.equals(service.getStatus())) {
                        LOG.info("Service {} was Stopped, trying the next one, if possible.", service.getName());
                        continue;
                    }
                    exchangeService.sendEmail(service, email, ruleName);

                    auditService.sendAuditMessage(AuditObjectTypeEnum.CUSTOM_RULE_ACTION, AuditOperationEnum.SEND_EMAIL, null, emailAddress, "UVMS");
                    return;
                }
            }
            LOG.info("No plugin of the correct type found. Nothing was sent.");
        } catch (JMSException e) {
            LOG.error("Failed to send email! {}", e.getMessage());
        }
    }

    public String createPollInternal(String assetGuid, String ruleName){
        MovementDetails fact = new MovementDetails();
        fact.setAssetGuid(assetGuid);
        fact.setAssetName(assetGuid);

        return createPollInternal(fact, ruleName);
    }

    private String createPollInternal(MovementDetails fact, String ruleName){
        try {
            String username = "Triggerd by rule: " + ruleName;
            String comment = "This poll was triggered by rule: " + ruleName + " on: " + Instant.now().toString() + " on Asset: " + fact.getAssetName();

            Response createdPollResponse = getWebTarget()
                    .path("internal/createPollForAsset")
                    .path(fact.getAssetGuid())
                    .queryParam("username", username)
                    .queryParam("comment", comment)
                    .request(MediaType.APPLICATION_JSON)
                    .header(HttpHeaders.AUTHORIZATION, tokenHandler.createAndFetchToken("user"))
                    .post(Entity.json(""), Response.class);

            if(createdPollResponse.getStatus() != 200){
                return "Unable to create poll";
            }

            CreatePollResultDto createPollResultDto = createdPollResponse.readEntity(CreatePollResultDto.class);
            if(!createPollResultDto.isUnsentPoll()){
                return createPollResultDto.getSentPolls().get(0);
            }else {
                return createPollResultDto.getUnsentPolls().get(0);
            }

        } catch (Exception e){
            LOG.error("Error while sending rule-triggered poll: ", e);
            return "NOK " + e.getMessage();
        }
    }

    private void createTicket(CustomRule customRule, MovementDetails fact) {
        try {
            Ticket ticket = new Ticket();

            ticket.setAssetGuid(fact.getAssetGuid());
            ticket.setMobileTerminalGuid(fact.getMobileTerminalGuid());
            ticket.setChannelGuid(fact.getChannelGuid());
            ticket.setCreatedDate(Instant.now());
            ticket.setUpdated(Instant.now());
            ticket.setRuleName(customRule.getName());
            ticket.setRuleGuid(customRule.getGuid().toString());
            ticket.setStatus(TicketStatusType.OPEN);
            ticket.setUpdatedBy("UVMS");
            ticket.setMovementGuid(fact.getMovementGuid());

            for (int i = 0; i < fact.getAreaTypes().size(); i++) {
                if ("EEZ".equals(fact.getAreaTypes().get(i))) {
                    ticket.setRecipient(fact.getAreaCodes().get(i));
                }
            }


            ticket.setTicketCount(1L);
            Ticket createdTicket = rulesDao.createTicket(ticket);

            ticketEvent.fire(new EventTicket(ticket, customRule));

            // Notify long-polling clients of the change (no value since FE will need to fetch it)
            ticketCountEvent.fire(new NotificationMessage("ticketCount", null));

            auditService.sendAuditMessage(AuditObjectTypeEnum.TICKET, AuditOperationEnum.CREATE, createdTicket.getGuid().toString(), null, createdTicket.getUpdatedBy());
        } catch (Exception e) { //TODO: figure out if we are to have this kind of exception handling here and if we are to catch everything
            LOG.error("[ Failed to create ticket! ] {}", e);
        }
    }


    public long getNumberOfOpenTickets(String userName) {
        LOG.info("Counting open tickets for user: {}", userName);
        List<UUID> validRuleGuids = rulesDao.getCustomRulesForTicketsByUser(userName);
        if (!validRuleGuids.isEmpty()) {
            List<String> validRuleStrings = new ArrayList<>();
            for (UUID uuid: validRuleGuids) {
                validRuleStrings.add(uuid.toString());
            }
            return rulesDao.getNumberOfOpenTickets(validRuleStrings);
        }
        return 0;
    }

    private Instant auditLog(String msg, Instant lastTimestamp) {
        Instant newTimestamp = Instant.now();
        long duration = newTimestamp.toEpochMilli() - lastTimestamp.toEpochMilli();
        LOG.debug("--> AUDIT - {} {}ms", msg, duration);
        return newTimestamp;
    }

    @Resource(name = "java:global/asset_endpoint")
    private String assetEndpoint;
    protected WebTarget getWebTarget() {
        Client client = ClientBuilder.newClient().register(JsonBConfigurator.class);
        return client.target(assetEndpoint);
    }

}
