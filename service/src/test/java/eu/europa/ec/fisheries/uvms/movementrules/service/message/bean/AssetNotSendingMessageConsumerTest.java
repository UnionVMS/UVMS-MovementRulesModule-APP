package eu.europa.ec.fisheries.uvms.movementrules.service.message.bean;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.RulesModuleMethod;
import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketStatusType;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageConstants;
import eu.europa.ec.fisheries.uvms.movementrules.model.dto.MovementDetails;
import eu.europa.ec.fisheries.uvms.movementrules.service.TransactionalTests;
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
import java.time.Instant;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

@RunWith(Arquillian.class)
public class AssetNotSendingMessageConsumerTest extends TransactionalTests {

    @Inject
    private CustomRulesEvaluator customRulesEvaluator;

    @Inject
    private RulesServiceBean rulesService;

    @Inject
    private RulesValidator rulesValidator;

    @Inject
    private RulesDao rulesDao;

    private JMSHelper jmsHelper = new JMSHelper();

//    @Before
//    public void clearExchangeQueue() throws Exception {
//        jmsHelper.clearQueue(MessageConstants.QUEUE_INCIDENT);
//    }

    @Test
    @OperateOnDeployment("normal")
    public void createAssetNotSendingEventAndConsumeIncidentQueue() throws Exception {
        rulesValidator.updateCustomRules();

        MovementDetails movementDetails = getMovementDetails();
        PreviousReport report = new PreviousReport();
        report.setAssetGuid(movementDetails.getAssetGuid());
        report.setMovementGuid(UUID.fromString(movementDetails.getMovementGuid()));
        report.setMobTermGuid(UUID.fromString(movementDetails.getMobileTerminalGuid()));
        rulesService.createAssetNotSendingTicket(ServiceConstants.ASSET_NOT_SENDING_RULE, report);

        Ticket ticket = rulesDao.getTicketByAssetAndRule(movementDetails.getAssetGuid(), ServiceConstants.ASSET_NOT_SENDING_RULE);
        assertNotNull(ticket);
        assertEquals(TicketStatusType.POLL_PENDING, ticket.getStatus());

//        customRulesEvaluator.evaluate(movementDetails);
//        Ticket closedTicket = rulesDao.getTicketByGuid(ticket.getGuid());
//        assertEquals(TicketStatusType.CLOSED, closedTicket.getStatus());

        Message message = jmsHelper.listenForResponseOnQueue(null, MessageConstants.QUEUE_INCIDENT);
        assertNotNull(message);
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
