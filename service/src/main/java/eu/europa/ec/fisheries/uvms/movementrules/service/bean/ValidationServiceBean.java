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

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import eu.europa.ec.fisheries.schema.exchange.movement.v1.MovementType;
import eu.europa.ec.fisheries.schema.exchange.movement.v1.MovementTypeType;
import eu.europa.ec.fisheries.schema.exchange.movement.v1.RecipientInfoType;
import eu.europa.ec.fisheries.schema.exchange.plugin.types.v1.EmailType;
import eu.europa.ec.fisheries.schema.exchange.plugin.types.v1.PluginType;
import eu.europa.ec.fisheries.schema.exchange.service.v1.ServiceResponseType;
import eu.europa.ec.fisheries.schema.exchange.service.v1.StatusType;
import eu.europa.ec.fisheries.schema.mobileterminal.polltypes.v1.PollMobileTerminal;
import eu.europa.ec.fisheries.schema.mobileterminal.polltypes.v1.PollRequestType;
import eu.europa.ec.fisheries.schema.mobileterminal.polltypes.v1.PollType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.ActionType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.SubscriptionTypeType;
import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketStatusType;
import eu.europa.ec.fisheries.uvms.commons.notifications.NotificationMessage;
import eu.europa.ec.fisheries.uvms.movementrules.model.dto.MovementDetails;
import eu.europa.ec.fisheries.uvms.movementrules.service.boundary.AuditServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.boundary.ExchangeServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.boundary.UserServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.constants.AuditObjectTypeEnum;
import eu.europa.ec.fisheries.uvms.movementrules.service.constants.AuditOperationEnum;
import eu.europa.ec.fisheries.uvms.movementrules.service.dao.RulesDao;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.RuleSubscription;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Ticket;
import eu.europa.ec.fisheries.uvms.movementrules.service.event.TicketCountEvent;
import eu.europa.ec.fisheries.uvms.movementrules.service.event.TicketEvent;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.ExchangeMovementMapper;
import eu.europa.ec.fisheries.uvms.rest.security.InternalRestTokenHandler;
import eu.europa.ec.fisheries.wsdl.user.module.GetContactDetailResponse;
import eu.europa.ec.fisheries.wsdl.user.types.Channel;
import eu.europa.ec.fisheries.wsdl.user.types.EndPoint;
import eu.europa.ec.fisheries.wsdl.user.types.Organisation;
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
    @TicketEvent
    private Event<NotificationMessage> ticketEvent;

    @Inject
    @TicketCountEvent
    private Event<NotificationMessage> ticketCountEvent;

    @EJB
    private InternalRestTokenHandler tokenHandler;

    // Triggered by rule engine
    public void customRuleTriggered(String ruleName, String ruleGuid, MovementDetails movementDetails, String actions) {
        LOG.info("Performing actions on triggered user rules, rule: {}", ruleName);

        Instant auditTimestamp = Instant.now();

        // Update last update 
        // TODO check if this is needed
        auditTimestamp = auditLog("Time to update last triggered:", auditTimestamp);

        // Always create a ticket
        createTicket(ruleName, ruleGuid, movementDetails);
        auditTimestamp = auditLog("Time to create ticket:", auditTimestamp);

        sendMailToSubscribers(ruleGuid, ruleName, movementDetails);
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
                    sendToEndpoint(ruleName, movementDetails, value, target);
                    auditTimestamp = auditLog("Time to send to endpoint:", auditTimestamp);
                    break;
                case MANUAL_POLL:
                    createManualPoll(movementDetails, ruleName);
                    auditTimestamp = auditLog("Time to send poll:", auditTimestamp);
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
    
    private void sendMailToSubscribers(String ruleGuid, String ruleName, MovementDetails movementDetails) {
        CustomRule customRule = null;
        try {
            customRule = rulesDao.getCustomRuleByGuid(UUID.fromString(ruleGuid));
        } catch (Exception e) {
            LOG.error("[ Failed to fetch rule when sending email to subscribers due to erro when getting CustomRule by GUID! ] {}", e.getMessage());
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
                        sendToEmail(emailAddress, ruleName, movementDetails);
                    } catch (Exception e) {
                        // If a mail attempt fails, proceed with the rest
                        LOG.error("Could not send email to user '{}'", subscription.getOwner());
                    }
                }
            }
        }
    }

    private void sendToEndpoint(String ruleName, MovementDetails movementDetails, String organisationName, String pluginName) {
        LOG.info("Sending to organisation '{}'", organisationName);

        try {
            MovementType exchangeMovement = ExchangeMovementMapper.mapToExchangeMovementType(movementDetails);

            Organisation organisation = userService.getOrganisation(organisationName);

            mapTypeOfMessage(exchangeMovement, movementDetails, organisation);

            String recipient = organisationName;
            List<RecipientInfoType> recipientInfo = new ArrayList<>();

            if (organisation != null) {
                recipient = organisation.getNation();
//                TODO if FLUX
//                recipient = recipientInfo.isEmpty() ? organisation.getNation() : recipientInfo.get(0).getValue();
                recipientInfo = getRecipientInfoType(organisation);
            }

            exchangeService.sendReportToPlugin(pluginName, ruleName, recipient, exchangeMovement, recipientInfo, movementDetails);

            auditService.sendAuditMessage(AuditObjectTypeEnum.CUSTOM_RULE_ACTION, AuditOperationEnum.SEND_TO_ENDPOINT, null, organisationName, "UVMS");

        } catch (JMSException e) {
            LOG.error("[ Failed to send to endpoint! ] {}", e.getMessage());
        }
    }

    private List<RecipientInfoType> getRecipientInfoType(Organisation organisation) {
        List<RecipientInfoType> recipientInfoList = new ArrayList<>();
        List<EndPoint> endPoints = organisation.getEndPoints();
        for (EndPoint endPoint : endPoints) {
            for (Channel channel : endPoint.getChannels()) {
                RecipientInfoType recipientInfo = new RecipientInfoType();
                recipientInfo.setKey(channel.getDataFlow());
                recipientInfo.setValue(endPoint.getUri());
                recipientInfoList.add(recipientInfo);
            }
        }
        return recipientInfoList;
    }

    private void mapTypeOfMessage(MovementType movement, MovementDetails movementDetails, Organisation organisation) {
        if (organisation == null) {
            return;
        }
        if (movementDetails.getEntAreaCodes().contains(organisation.getNation())) {
            movement.setMovementType(MovementTypeType.ENT);
        } else if (movementDetails.getExtAreaCodes().contains(organisation.getNation()) ) {
            movement.setMovementType(MovementTypeType.EXI);
        }
    }
    
    private void sendToEmail(String emailAddress, String ruleName, MovementDetails movementDetails) {
        LOG.info("Sending email to '{}'", emailAddress);

        EmailType email = new EmailType();

        email.setSubject(buildSubject(ruleName));
        email.setBody(buildBody(ruleName, movementDetails));
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

    private String buildSubject(String ruleName) {
        StringBuilder subjectBuilder = new StringBuilder();
        subjectBuilder.append("Rule '")
                .append(ruleName)
                .append("' has been triggered.");
        return subjectBuilder.toString();
    }
    
    private String buildBody(String ruleName, MovementDetails movementDetails) {
        StringBuilder sb = new StringBuilder();
        sb.append("<html>")
                .append(buildSubject(ruleName))
                .append("<br><br>")
                .append(buildAssetBodyPart(movementDetails.getAssetName(), movementDetails.getIrcs(), movementDetails.getCfr()))
                .append(buildPositionBodyPart(movementDetails))
                .append("</html>");

        return sb.toString();
    }

    private String buildAssetBodyPart(String assetName, String ircs, String cfr) {
        StringBuilder assetBuilder = new StringBuilder();
        assetBuilder.append("<b>Asset:</b>")
                .append("<br>&nbsp;&nbsp;")
                .append("Name: ")
                .append(assetName)
                .append("<br>&nbsp;&nbsp;")
                .append("IRCS: ")
                .append(ircs)
                .append("<br>&nbsp;&nbsp;")
                .append("CFR: ")
                .append(cfr)
                .append("<br>");

        return assetBuilder.toString();
    }

    private String buildPositionBodyPart(MovementDetails fact) {
        StringBuilder positionBuilder = new StringBuilder();
        positionBuilder.append("<b>Position report:</b>")
                .append("<br>&nbsp;&nbsp;")
                .append("Report timestamp: ")
                .append(fact.getPositionTime())
                .append("<br>&nbsp;&nbsp;")
                .append("Longitude: ")
                .append(fact.getLongitude())
                .append("<br>&nbsp;&nbsp;")
                .append("Latitude: ")
                .append(fact.getLatitude())
                .append("<br>&nbsp;&nbsp;")
                .append("Status code: ")
                .append(fact.getStatusCode())
                .append("<br>&nbsp;&nbsp;")
                .append("Reported speed: ")
                .append(fact.getReportedSpeed())
                .append("<br>&nbsp;&nbsp;")
                .append("Reported course: ")
                .append(fact.getReportedCourse())
                .append("<br>&nbsp;&nbsp;")
                .append("Calculated speed: ")
                .append(fact.getCalculatedSpeed())
                .append("<br>&nbsp;&nbsp;")
                .append("Calculated course: ")
                .append(fact.getCalculatedCourse())
                .append("<br>&nbsp;&nbsp;")
                .append("Com channel type: ")
                .append(fact.getComChannelType())
                .append("<br>&nbsp;&nbsp;")
                .append("Segment type: ")
                .append(fact.getSegmentType())
                .append("<br>&nbsp;&nbsp;")
                .append("Source: ")
                .append(fact.getSource())
                .append("<br>&nbsp;&nbsp;")
                .append("Movement type: ")
                .append(fact.getMovementType())
                .append("<br>&nbsp;&nbsp;")
                .append("Activity type: ")
                .append(fact.getActivityMessageType())
                .append("<br>&nbsp;&nbsp;")
                .append("Closest port: ")
                .append(fact.getClosestPortCode())
                .append("<br>&nbsp;&nbsp;")
                .append("Closest country: ")
                .append(fact.getClosestCountryCode())
                .append("<br>&nbsp;&nbsp;");

        positionBuilder.append("Areas:");
        for (int i = 0; i < fact.getAreaCodes().size(); i++) {
            positionBuilder.append("<br>&nbsp;&nbsp;&nbsp;&nbsp;")
                    .append(fact.getAreaCodes().get(i))
                    .append(" (")
                    .append(fact.getAreaTypes().get(i))
                    .append(")");
        }

        return positionBuilder.toString();
    }

    private String createManualPoll(MovementDetails fact, String ruleName){
        try {
            PollRequestType poll = new PollRequestType();
            poll.setUserName("Triggerd by rule: " + ruleName);
            poll.setComment("This poll was triggered by rule: " + ruleName + " on: " + Instant.now().toString() + " on Asset: " + fact.getAssetName());
            poll.setPollType(PollType.MANUAL_POLL);

            PollMobileTerminal pmt = new PollMobileTerminal();
            pmt.setComChannelId(fact.getChannelGuid());
            pmt.setConnectId(fact.getAssetGuid());
            pmt.setMobileTerminalId(fact.getMobileTerminalGuid());
            poll.getMobileTerminals().add(pmt);

            Response createdPoll = getWebTarget()
                    .path("internal/poll")
                    .request(MediaType.APPLICATION_JSON)
                    .header(HttpHeaders.AUTHORIZATION, tokenHandler.createAndFetchToken("user"))
                    .post(Entity.json(poll), Response.class);

        if(createdPoll.getStatus() != 200){
            return "NOK";
        }
        return "OK";
        } catch (Exception e){
            LOG.error("Error while sending rule-triggered poll: ", e);
            return "NOK";
        }
    }

    private void createTicket(String ruleName, String ruleGuid, MovementDetails fact) {
        try {
            Ticket ticket = new Ticket();

            ticket.setAssetGuid(fact.getAssetGuid());
            ticket.setMobileTerminalGuid(fact.getMobileTerminalGuid());
            ticket.setChannelGuid(fact.getChannelGuid());
            ticket.setCreatedDate(Instant.now());
            ticket.setUpdated(Instant.now());
            ticket.setRuleName(ruleName);
            ticket.setRuleGuid(ruleGuid);
            ticket.setStatus(TicketStatusType.OPEN.value());
            ticket.setUpdatedBy("UVMS");
            ticket.setMovementGuid(fact.getMovementGuid());
            //ticket.setGuid(UUID.randomUUID().toString());

            for (int i = 0; i < fact.getAreaTypes().size(); i++) {
                if ("EEZ".equals(fact.getAreaTypes().get(i))) {
                    ticket.setRecipient(fact.getAreaCodes().get(i));
                }
            }


            ticket.setTicketCount(1L);
            Ticket createdTicket = rulesDao.createTicket(ticket);


            ticketEvent.fire(new NotificationMessage("guid", createdTicket.getGuid()));

            // Notify long-polling clients of the change (no value since FE will need to fetch it)
            ticketCountEvent.fire(new NotificationMessage("ticketCount", null));

            auditService.sendAuditMessage(AuditObjectTypeEnum.TICKET, AuditOperationEnum.CREATE, createdTicket.getGuid().toString(), null, createdTicket.getUpdatedBy());
        } catch (Exception e) { //TODO: figure out if we are to have this kind of exception handling here and if we are to catch everything
            LOG.error("[ Failed to create ticket! ] {}", e);
            LOG.error("[ERROR] Error when creating ticket {}", e);
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
        LOG.info("--> AUDIT - {} {}ms", msg, duration);
        return newTimestamp;
    }

    @Resource(name = "java:global/asset_endpoint")
    private String assetEndpoint;
    protected WebTarget getWebTarget() {
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
        objectMapper.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        Client client = ClientBuilder.newClient();
        return client.target(assetEndpoint);
    }

}
