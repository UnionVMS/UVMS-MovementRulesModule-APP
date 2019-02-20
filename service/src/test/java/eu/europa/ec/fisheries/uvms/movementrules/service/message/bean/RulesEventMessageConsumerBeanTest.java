package eu.europa.ec.fisheries.uvms.movementrules.service.message.bean;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;

import java.time.Instant;
import java.util.UUID;
import javax.jms.Message;
import javax.jms.TextMessage;
import org.jboss.arquillian.container.test.api.OperateOnDeployment;
import org.jboss.arquillian.container.test.api.RunAsClient;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.PingRequest;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.PingResponse;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.RulesModuleMethod;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageConstants;
import eu.europa.ec.fisheries.uvms.movementrules.model.dto.MovementDetails;
import eu.europa.ec.fisheries.uvms.movementrules.model.mapper.JAXBMarshaller;
import eu.europa.ec.fisheries.uvms.movementrules.service.BuildRulesServiceDeployment;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.JMSHelper;

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
        final String RESPONSE_QUEUE = "IntegrationTestsResponseQueue";
        PingRequest request = new PingRequest();
        request.setMethod(RulesModuleMethod.PING);
        String requestString = JAXBMarshaller.marshallJaxBObjectToString(request);
        String correlationId = jmsHelper.sendMessageToRules(requestString, RulesModuleMethod.PING.value(), RESPONSE_QUEUE);
        Message message = jmsHelper.listenForResponseOnQueue(correlationId, RESPONSE_QUEUE);
        assertThat(message, is(notNullValue()));

        PingResponse pingResponse = JAXBMarshaller.unmarshallTextMessage((TextMessage) message, PingResponse.class);
        assertThat(pingResponse.getResponse(), is("pong"));
    }

    // Sanity test
    @Ignore
    @Test
    @OperateOnDeployment("normal")
    public void evaluateCustomRulesVoidTest() throws Exception {
        MovementDetails movementDetails = new MovementDetails();
        movementDetails.setMovementGuid(UUID.randomUUID().toString());
        movementDetails.setLatitude(11d);
        movementDetails.setLongitude(56d);
        movementDetails.setPositionTime(Instant.now());
        movementDetails.setSource("INMARSAT_C");
        movementDetails.setAssetGuid(UUID.randomUUID().toString());
        movementDetails.setFlagState("SWE");
        
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
        jmsHelper.sendMessageToRules(objectMapper.writeValueAsString(movementDetails), RulesModuleMethod.EVALUATE_RULES.value(), MessageConstants.QUEUE_EXCHANGE_EVENT_NAME);
        
        // wait for message to be processed
        Thread.sleep(5000);
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
