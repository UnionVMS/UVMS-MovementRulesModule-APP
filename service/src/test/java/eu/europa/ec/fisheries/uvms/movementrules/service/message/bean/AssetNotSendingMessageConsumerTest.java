package eu.europa.ec.fisheries.uvms.movementrules.service.message.bean;

import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketStatusType;
import eu.europa.ec.fisheries.uvms.movementrules.model.dto.MovementDetails;
import eu.europa.ec.fisheries.uvms.movementrules.service.BuildRulesServiceDeployment;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.CustomRulesEvaluator;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.RulesServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.RulesValidator;
import eu.europa.ec.fisheries.uvms.movementrules.service.constants.ServiceConstants;
import eu.europa.ec.fisheries.uvms.movementrules.service.dao.RulesDao;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.PreviousReport;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Ticket;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.JMSHelper;
import org.jboss.arquillian.container.test.api.OperateOnDeployment;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import javax.inject.Inject;
import javax.jms.Message;
import javax.jms.TextMessage;
import java.time.Instant;
import java.util.UUID;

import static org.junit.Assert.*;

@RunWith(Arquillian.class)
public class AssetNotSendingMessageConsumerTest extends BuildRulesServiceDeployment {

    @Inject
    private RulesServiceBean rulesService;

    @Inject
    private RulesValidator rulesValidator;

    @Inject
    private RulesDao rulesDao;

    @Inject
    private CustomRulesEvaluator customRulesEvaluator;

    private JMSHelper jmsHelper = new JMSHelper();

    private static final String QUEUE_NAME = "IncidentEvent";

    @Before
    public void clearExchangeQueue() throws Exception {
        jmsHelper.clearQueue(QUEUE_NAME);
    }

    @Test
    @OperateOnDeployment("normal")
    public void createAssetNotSendingEventAndConsumeIncidentQueue() throws Exception {
        rulesValidator.updateCustomRules();

        MovementDetails movementDetails = getMovementDetails();
        PreviousReport report = new PreviousReport();
        report.setAssetGuid(movementDetails.getAssetGuid());
        report.setMovementGuid(UUID.fromString(movementDetails.getMovementGuid()));
        report.setMobTermGuid(UUID.fromString(movementDetails.getMobileTerminalGuid()));
        rulesService.timerRuleTriggered(ServiceConstants.ASSET_NOT_SENDING_RULE, report);

        Ticket ticket = rulesDao.getTicketByAssetAndRule(movementDetails.getAssetGuid(), ServiceConstants.ASSET_NOT_SENDING_RULE);
        assertNotNull(ticket);
        assertEquals(TicketStatusType.POLL_PENDING, ticket.getStatus());

        // AssetNotSending Create Event
        Message message1 = jmsHelper.listenForResponseOnQueue(null, QUEUE_NAME);
        assertNotNull(message1);

        // Update ticket
        customRulesEvaluator.evaluate(movementDetails);
        Ticket closedTicket = rulesDao.getTicketByGuid(ticket.getGuid());
        assertEquals(TicketStatusType.CLOSED, closedTicket.getStatus());

        // AssetNotSending Update Event
        Message message2 = jmsHelper.listenForResponseOnQueue(null, QUEUE_NAME);
        assertNotNull(message2);
    }

    @Test
    @OperateOnDeployment("normal")
    public void createAssetSendingDespiteBeingLongTermParkedTest() throws Exception {
        rulesValidator.updateCustomRules();

        MovementDetails movementDetails = getMovementDetails();
        movementDetails.setLongTermParked(true);
        customRulesEvaluator.evaluate(movementDetails);

        // sending despite Create Event
        TextMessage message = (TextMessage) jmsHelper.listenForResponseOnQueue(null, QUEUE_NAME);
        assertNotNull(message);

        assertTrue(message.getText().contains(ServiceConstants.ASSET_SENDING_DESPITE_LONG_TERM_PARKED_RULE));
        assertTrue(message.getText().contains(movementDetails.getAssetGuid()));
    }


    private MovementDetails getMovementDetails() {
        MovementDetails movementDetails = new MovementDetails();
        movementDetails.setMovementGuid(UUID.randomUUID().toString());
        movementDetails.setMobileTerminalGuid(UUID.randomUUID().toString());
        movementDetails.setLatitude(11d);
        movementDetails.setLongitude(56d);
        movementDetails.setPositionTime(Instant.now());
        movementDetails.setSource("INMARSAT_C");
        movementDetails.setAssetGuid(UUID.randomUUID().toString());
        movementDetails.setFlagState("SWE");
        return movementDetails;
    }
}
