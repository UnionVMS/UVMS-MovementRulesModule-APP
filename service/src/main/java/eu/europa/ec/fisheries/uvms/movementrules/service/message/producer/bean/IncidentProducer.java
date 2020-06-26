package eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.bean;

import eu.europa.ec.fisheries.uvms.commons.date.JsonBConfigurator;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageConstants;
import eu.europa.ec.fisheries.uvms.incident.model.dto.IncidentTicketDto;
import eu.europa.ec.fisheries.uvms.incident.model.dto.enums.IncidentType;
import eu.europa.ec.fisheries.uvms.movementrules.service.constants.ServiceConstants;
import eu.europa.ec.fisheries.uvms.movementrules.service.dto.EventTicket;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Ticket;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.jms.*;
import javax.json.bind.Jsonb;

@Stateless
public class IncidentProducer {

    private static final Logger LOG = LoggerFactory.getLogger(IncidentProducer.class);

    @Inject
    @JMSConnectionFactory("java:/ConnectionFactory")
    private JMSContext context;

    @Resource(mappedName = "java:/" + MessageConstants.QUEUE_INCIDENT)
    private Destination queue;

    private Jsonb jsonb;

    @PostConstruct
    public void init() {
        JsonBConfigurator configurator = new JsonBConfigurator();
        jsonb = configurator.getContext(null);
    }

    public void updatedTicket(EventTicket eventTicket) {
        IncidentType incidentType = determineIncidentType(eventTicket);
        IncidentTicketDto dto = mapToIncidentTicket(eventTicket, incidentType);
        send(dto, "IncidentUpdate");
    }

    public void createdTicket(EventTicket eventTicket) {
        IncidentType incidentType = determineIncidentType(eventTicket);
        IncidentTicketDto dto = mapToIncidentTicket(eventTicket, incidentType);
        send(dto, "Incident");
    }

    public void send(IncidentTicketDto ticket, String eventName) {
        try {
            String json = jsonb.toJson(ticket);
            TextMessage message = context.createTextMessage(json);
            message.setStringProperty("eventName", eventName);
            JMSProducer producer = context.createProducer();
            producer.setDeliveryMode(DeliveryMode.PERSISTENT).send(queue, message);
        } catch (Exception e) {
            LOG.error("Error while sending AssetNotSending event. {}", e.toString());
        }
    }

    private IncidentType determineIncidentType(EventTicket eventTicket){
        if(eventTicket.getTicket().getRuleGuid().equals(ServiceConstants.ASSET_NOT_SENDING_RULE)){
            return IncidentType.ASSET_NOT_SENDING;
        } else if(eventTicket.getTicket().getRuleGuid().equals(ServiceConstants.ASSET_SENDING_DESPITE_LONG_TERM_PARKED_RULE)){
            return IncidentType.LONG_TERM_PARKED;
        } else {
            return IncidentType.ASSET_SENDING_NORMAL;   //not really a lot of choice here.....
        }
    }

    private IncidentTicketDto mapToIncidentTicket(EventTicket eventTicket, IncidentType ticketType){
        IncidentTicketDto dto = new IncidentTicketDto();
        Ticket ticket = eventTicket.getTicket();
        dto.setAssetId(ticket.getAssetGuid());
        dto.setChannelId(ticket.getChannelGuid());
        dto.setCreatedDate(ticket.getCreatedDate());
        dto.setId(ticket.getGuid());
        dto.setMobTermId(ticket.getMobileTerminalGuid());
        dto.setMovementId(ticket.getMovementGuid());
        dto.setPollId(eventTicket.getPollId());
        dto.setRecipient(ticket.getRecipient());
        dto.setRuleGuid(ticket.getRuleGuid());
        dto.setRuleName(ticket.getRuleName());
        dto.setStatus(ticket.getStatus().value());
        dto.setTicketCount(ticket.getTicketCount());
        dto.setType(ticketType);
        dto.setUpdated(ticket.getUpdated());
        dto.setUpdatedBy(ticket.getUpdatedBy());

        return dto;
    }
}
