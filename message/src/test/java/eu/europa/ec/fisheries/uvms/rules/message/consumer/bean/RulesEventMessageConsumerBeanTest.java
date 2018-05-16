package eu.europa.ec.fisheries.uvms.rules.message.consumer.bean;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;
import javax.jms.Message;
import javax.jms.TextMessage;
import org.jboss.arquillian.container.test.api.RunAsClient;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Test;
import org.junit.runner.RunWith;
import eu.europa.ec.fisheries.schema.rules.module.v1.PingRequest;
import eu.europa.ec.fisheries.schema.rules.module.v1.PingResponse;
import eu.europa.ec.fisheries.schema.rules.module.v1.RulesModuleMethod;
import eu.europa.ec.fisheries.uvms.rules.message.AbstractMessageTest;
import eu.europa.ec.fisheries.uvms.rules.message.JMSHelper;
import eu.europa.ec.fisheries.uvms.rules.model.mapper.JAXBMarshaller;

@RunWith(Arquillian.class)
public class RulesEventMessageConsumerBeanTest extends AbstractMessageTest {

    private JMSHelper jmsHelper = new JMSHelper();
    
    @Test
    @RunAsClient
    public void pingTest() throws Exception {
        PingRequest request = new PingRequest();
        request.setMethod(RulesModuleMethod.PING);
        String requestString = JAXBMarshaller.marshallJaxBObjectToString(request);
        String correlationId = jmsHelper.sendAssetMessage(requestString);
        Message message = jmsHelper.listenForResponse(correlationId);
        assertThat(message, is(notNullValue()));
        
        PingResponse pingResponse = JAXBMarshaller.unmarshallTextMessage((TextMessage) message, PingResponse.class);
        assertThat(pingResponse.getResponse(), is("pong"));
    }

}
