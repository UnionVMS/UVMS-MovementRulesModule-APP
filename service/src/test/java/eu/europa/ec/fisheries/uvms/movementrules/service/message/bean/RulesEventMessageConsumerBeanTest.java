package eu.europa.ec.fisheries.uvms.movementrules.service.message.bean;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

import java.util.Calendar;
import java.util.TimeZone;
import javax.ejb.EJB;
import javax.inject.Inject;
import javax.jms.Message;
import javax.jms.TextMessage;

import eu.europa.ec.fisheries.uvms.movementrules.service.BuildRulesServiceDeployment;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.JMSHelper;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.TestHelper;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.constants.DataSourceQueue;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.RulesMessageProducer;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.bean.ExchangeProducerBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.bean.RulesMessageProducerBean;
import org.jboss.arquillian.container.test.api.OperateOnDeployment;
import org.jboss.arquillian.container.test.api.RunAsClient;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import eu.europa.ec.fisheries.schema.exchange.module.v1.ProcessedMovementResponse;
import eu.europa.ec.fisheries.schema.exchange.movement.v1.MovementBaseType;
import eu.europa.ec.fisheries.schema.exchange.movement.v1.MovementRefTypeType;
import eu.europa.ec.fisheries.schema.movementrules.exchange.v1.PluginType;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.PingRequest;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.PingResponse;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.RulesModuleMethod;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.RawMovementType;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageConstants;
import eu.europa.ec.fisheries.uvms.movementrules.model.mapper.JAXBMarshaller;
import eu.europa.ec.fisheries.uvms.movementrules.model.mapper.MovementRulesModuleRequestMapper;

@RunAsClient
@RunWith(Arquillian.class)
public class RulesEventMessageConsumerBeanTest extends BuildRulesServiceDeployment {

    private JMSHelper jmsHelper = new JMSHelper();

    @Before
    public void clearExchangeQueue() throws Exception {
        jmsHelper.clearQueue(MessageConstants.QUEUE_EXCHANGE_EVENT_NAME);
        jmsHelper.clearQueue(MessageConstants.QUEUE_MOVEMENTRULES_EVENT_NAME);
    }

    @Test
    @OperateOnDeployment("normal")
    public void pingTest() throws Exception {
        final String RESPONSE_QUEUE = "RulesTestQueue";
        PingRequest request = new PingRequest();
        request.setMethod(RulesModuleMethod.PING);
        String requestString = JAXBMarshaller.marshallJaxBObjectToString(request);
        String correlationId = jmsHelper.sendMessageToRules(requestString, RulesModuleMethod.PING.value(), RESPONSE_QUEUE);
        Message message = jmsHelper.listenForResponseOnQueue(correlationId, RESPONSE_QUEUE);
        assertThat(message, is(notNullValue()));

        PingResponse pingResponse = JAXBMarshaller.unmarshallTextMessage((TextMessage) message, PingResponse.class);
        assertThat(pingResponse.getResponse(), is("pong"));
    }
/*
    @Inject
    private ExchangeProducerBean producer;

    @Test
    @OperateOnDeployment("normal")
    public void jmsSanityCheck() throws Exception{
        System.out.println("Now");
        Thread.sleep(5 * 60 * 1000);
        String corr = producer.sendModuleMessage("test text", "PROCESSED_MOVEMENT");

        Message message = jmsHelper.listenForResponseOnQueue(corr, MessageConstants.QUEUE_EXCHANGE_EVENT_NAME);
        assertNotNull(message);
    }*/


    @Test
    @OperateOnDeployment("normal")
    public void setMovementReportTest() throws Exception {
        RawMovementType movement = TestHelper.createBasicMovement();
        String request = MovementRulesModuleRequestMapper.createSetMovementReportRequest(PluginType.NAF, movement, "testUser");
        String correlationId = jmsHelper.sendMessageToRules(request, RulesModuleMethod.SET_MOVEMENT_REPORT.value(), MessageConstants.QUEUE_EXCHANGE_EVENT_NAME);
        Message responseMessage = jmsHelper.listenForResponseOnQueue(correlationId, MessageConstants.QUEUE_EXCHANGE_EVENT_NAME);

        ProcessedMovementResponse movementResponse = JAXBMarshaller.unmarshallTextMessage((TextMessage) responseMessage, ProcessedMovementResponse.class);
        assertThat(movementResponse.getMovementRefType().getType(), is(MovementRefTypeType.MOVEMENT));

        MovementBaseType returnedMovement = movementResponse.getOrgRequest().getMovement();
        assertThat(returnedMovement.getAssetId().getAssetIdList().get(0).getValue(), is(movement.getAssetId().getAssetIdList().get(0).getValue()));
    }

    @Test
    @OperateOnDeployment("normal")
    public void setMovementReportCreateTwoPositionsTest() throws Exception {
        RawMovementType movement = TestHelper.createBasicMovement();
        String request = MovementRulesModuleRequestMapper.createSetMovementReportRequest(PluginType.NAF, movement, "testUser");
        String correlationId = jmsHelper.sendMessageToRules(request, RulesModuleMethod.SET_MOVEMENT_REPORT.value(), MessageConstants.QUEUE_EXCHANGE_EVENT_NAME);
        Message responseMessage = jmsHelper.listenForResponseOnQueue(correlationId, MessageConstants.QUEUE_EXCHANGE_EVENT_NAME);

        ProcessedMovementResponse movementResponse = JAXBMarshaller.unmarshallTextMessage((TextMessage) responseMessage, ProcessedMovementResponse.class);
        MovementBaseType returnedMovement = movementResponse.getOrgRequest().getMovement();

        assertThat(returnedMovement.getAssetId().getAssetIdList().get(0).getValue(), is(movement.getAssetId().getAssetIdList().get(0).getValue()));

        RawMovementType movement2 = TestHelper.createBasicMovement();
        request = MovementRulesModuleRequestMapper.createSetMovementReportRequest(PluginType.NAF, movement2, "testUser");
        correlationId = jmsHelper.sendMessageToRules(request, RulesModuleMethod.SET_MOVEMENT_REPORT.value(), MessageConstants.QUEUE_EXCHANGE_EVENT_NAME);
        responseMessage = jmsHelper.listenForResponseOnQueue(correlationId, MessageConstants.QUEUE_EXCHANGE_EVENT_NAME);

        ProcessedMovementResponse movementResponse2 = JAXBMarshaller.unmarshallTextMessage((TextMessage) responseMessage, ProcessedMovementResponse.class);
        MovementBaseType returnedMovement2 = movementResponse2.getOrgRequest().getMovement();

        assertThat(returnedMovement2.getAssetId().getAssetIdList().get(0).getValue(), is(movement2.getAssetId().getAssetIdList().get(0).getValue()));
    }

    @Test
    @OperateOnDeployment("normal")
    public void setMovementReportNullLatitudeShouldTriggerSanityRuleTest() throws Exception {
        RawMovementType movement = TestHelper.createBasicMovement();
        movement.getPosition().setLatitude(null);
        String request = MovementRulesModuleRequestMapper.createSetMovementReportRequest(PluginType.NAF, movement, "testUser");
        String correlationId = jmsHelper.sendMessageToRules(request, RulesModuleMethod.SET_MOVEMENT_REPORT.value(), MessageConstants.QUEUE_EXCHANGE_EVENT_NAME);
        Message responseMessage = jmsHelper.listenForResponseOnQueue(correlationId, MessageConstants.QUEUE_EXCHANGE_EVENT_NAME);

        ProcessedMovementResponse movementResponse = JAXBMarshaller.unmarshallTextMessage((TextMessage) responseMessage, ProcessedMovementResponse.class);
        assertThat(movementResponse.getMovementRefType().getType(), is(MovementRefTypeType.ALARM));
    }

    @Test
    @OperateOnDeployment("normal")
    public void setMovementReportFutureDateShouldTriggerSanityRuleTest() throws Exception {
        RawMovementType movement = TestHelper.createBasicMovement();
        Calendar calendar = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
        calendar.add(Calendar.HOUR, 1);
        movement.setPositionTime(calendar.getTime());
        String request = MovementRulesModuleRequestMapper.createSetMovementReportRequest(PluginType.NAF, movement, "testUser");
        String correlationId = jmsHelper.sendMessageToRules(request, RulesModuleMethod.SET_MOVEMENT_REPORT.value(), MessageConstants.QUEUE_EXCHANGE_EVENT_NAME);
        Message responseMessage = jmsHelper.listenForResponseOnQueue(correlationId, MessageConstants.QUEUE_EXCHANGE_EVENT_NAME);

        ProcessedMovementResponse movementResponse = JAXBMarshaller.unmarshallTextMessage((TextMessage) responseMessage, ProcessedMovementResponse.class);
        assertThat(movementResponse.getMovementRefType().getType(), is(MovementRefTypeType.ALARM));
    }
    
    /*
    @Test
    @RunAsClient
    public void getTicketsAndRulesByMovementsTest() throws Exception {
        String request = RulesModuleRequestMapper.createGetTicketsAndRulesByMovementsRequest(new ArrayList<String>()); 
        String correlationId = jmsHelper.sendMessageToRules(request);
        Message response = jmsHelper.listenForResponse(correlationId);
        assertThat(response, is(notNullValue()));
    }
    */
}
