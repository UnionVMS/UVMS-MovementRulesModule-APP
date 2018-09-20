package eu.europa.ec.fisheries.uvms.movementrules.service.message.bean;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.powermock.api.mockito.PowerMockito.mock;
import java.util.UUID;
import javax.enterprise.event.Event;
import javax.jms.TextMessage;

import eu.europa.ec.fisheries.uvms.movementrules.service.bean.MovementReportProcessorBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.RulesEventServiceBean;
import org.hamcrest.CoreMatchers;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Matchers;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.powermock.modules.junit4.PowerMockRunner;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.CustomRuleType;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketsAndRulesByMovementsResponse;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.RawMovementType;
import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketType;
import eu.europa.ec.fisheries.schema.movementrules.ticketrule.v1.TicketAndRuleType;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageException;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.event.carrier.EventMessage;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.RulesMessageProducer;
import eu.europa.ec.fisheries.uvms.movementrules.service.RulesService;

//@RunWith(PowerMockRunner.class)
public class RulesEventServiceBeanTest {

    //TODO: Move the important parts of this into a proper JMS testing class
    /*@Mock
    private Event<EventMessage> errorEvent;
    @Mock
    private RulesMessageProducer rulesProducer;
    @Mock
    private RulesService rulesService;
    @Mock
    private MovementReportProcessorBean movementReportBean;
   
    @InjectMocks
    RulesEventServiceBean rulesEventService;
   
    @Before
    public void initMocks() {
        MockitoAnnotations.initMocks(this);
    }
    
    @Test
    public void pingReceivedTest() throws Exception {
        TextMessage textMessage = mock(TextMessage.class);
        EventMessage eventMessage = new EventMessage(textMessage);
        rulesEventService.pingReceived(eventMessage);
        ArgumentCaptor<String> argumentCaptor = ArgumentCaptor.forClass(String.class);
        verify(rulesProducer).sendModuleResponseMessage(Matchers.any(TextMessage.class), argumentCaptor.capture());
        
        String capturedArgument = argumentCaptor.getValue();
        
        String expected = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n" + 
                          "<PingResponse>\n" + 
                          "    <response>pong</response>\n" + 
                          "</PingResponse>\n";
        assertThat(capturedArgument, is(expected));
    }
    
    @Test
    public void pingReceivedNegativeTest() throws Exception {
        TextMessage textMessage = mock(TextMessage.class);
        EventMessage eventMessage = new EventMessage(textMessage);
        doThrow(MessageException.class).when(rulesProducer).sendModuleResponseMessage(Matchers.anyObject(), Matchers.anyString());
        
        rulesEventService.pingReceived(eventMessage);
        
        ArgumentCaptor<EventMessage> argumentCaptor = ArgumentCaptor.forClass(EventMessage.class);
        verify(errorEvent).fire(argumentCaptor.capture());
        
        EventMessage capturedArgument = argumentCaptor.getValue();
        assertThat(capturedArgument, is(eventMessage));
    }

    @Test
    public void setMovementReportReceivedTest() throws Exception {
        String lat = "1.0";
        String lon = "2.0";
        String request = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n" + 
                "<SetMovementReportRequest>\n" + 
                "    <method>SET_MOVEMENT_REPORT</method>\n" + 
                "    <username>user</username>\n" + 
                "    <type>NAF</type>\n" + 
                "    <request>\n" + 
                "        <position>\n" + 
                "            <longitude>" + lon + "</longitude>" + 
                "            <latitude>" + lat + "</latitude>\n" + 
                "            <altitude xsi:nil=\"true\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"/>\n" + 
                "        </position>\n" + 
                "        <reportedSpeed xsi:nil=\"true\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"/>\n" + 
                "        <reportedCourse xsi:nil=\"true\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"/>\n" + 
                "        <tripNumber xsi:nil=\"true\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"/>\n" + 
                "    </request>\n" + 
                "</SetMovementReportRequest>";
        TextMessage textMessage = mock(TextMessage.class);
        when(textMessage.getText()).thenReturn(request);
        EventMessage eventMessage = new EventMessage(textMessage);
        
        rulesEventService.setMovementReportReceived(eventMessage);
        ArgumentCaptor<RawMovementType> argumentCaptor = ArgumentCaptor.forClass(RawMovementType.class);
        verify(movementReportBean).setMovementReportReceived(argumentCaptor.capture(), Matchers.any(String.class), Matchers.any(String.class));
        RawMovementType value = argumentCaptor.getValue();
        assertThat(value.getPosition().getLatitude(), is(Double.valueOf(lat)));
        assertThat(value.getPosition().getLongitude(), is(Double.valueOf(lon)));
    }
    
    @Test
    public void getTicketsAndRulesByMovementsEventTest() throws Exception {
        String guid = UUID.randomUUID().toString();
        String request = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n" + 
                         "<GetTicketsAndRulesByMovementsRequest>\n" + 
                         "    <method>GET_TICKETS_AND_RULES_BY_MOVEMENTS</method>\n" +
                         "    <movementGuids>" + guid + "</movementGuids>\n" + 
                         "</GetTicketsAndRulesByMovementsRequest>\n";
        TextMessage textMessage = mock(TextMessage.class);
        when(textMessage.getText()).thenReturn(request);
        EventMessage eventMessage = new EventMessage(textMessage);
        
        GetTicketsAndRulesByMovementsResponse response = new GetTicketsAndRulesByMovementsResponse();
        TicketAndRuleType ticketAndRule = new TicketAndRuleType();
        CustomRuleType rule = new CustomRuleType();
        String name = "RULE";
        rule.setName(name);
        ticketAndRule.setRule(rule);
        ticketAndRule.setTicket(new TicketType());
        response.getTicketsAndRules().add(ticketAndRule);
        when(rulesService.getTicketsAndRulesByMovements(Matchers.anyListOf(String.class))).thenReturn(response);
        
        rulesEventService.getTicketsAndRulesByMovementsEvent(eventMessage);
        ArgumentCaptor<String> argumentCaptor = ArgumentCaptor.forClass(String.class);
        verify(rulesProducer).sendModuleResponseMessage(Matchers.any(TextMessage.class), argumentCaptor.capture());
        
        String expected = "<name>" + name + "</name>\n";
        assertThat(argumentCaptor.getValue(), CoreMatchers.containsString(expected));
    }
    
    @Test
    public void getTicketsAndRulesByMovementsEventNegativeTest() throws Exception {
        String guid = UUID.randomUUID().toString();
        String request = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n" + 
                         "<GetTicketsAndRulesByMovementsRequest>\n" + 
                         "    <method>GET_TICKETS_AND_RULES_BY_MOVEMENTS</method>\n" +
                         "    <movementGuids>" + guid + "</movementGuids>\n" + 
                         "</GetTicketsAndRulesByMovementsRequest>\n";
        TextMessage textMessage = mock(TextMessage.class);
        when(textMessage.getText()).thenReturn(request);
        EventMessage eventMessage = new EventMessage(textMessage);
        
        GetTicketsAndRulesByMovementsResponse response = new GetTicketsAndRulesByMovementsResponse();
        TicketAndRuleType ticketAndRule = new TicketAndRuleType();
        CustomRuleType rule = new CustomRuleType();
        String name = "RULE";
        rule.setName(name);
        ticketAndRule.setRule(rule);
        ticketAndRule.setTicket(new TicketType());
        response.getTicketsAndRules().add(ticketAndRule);
        
        when(rulesService.getTicketsAndRulesByMovements(Matchers.anyListOf(String.class))).thenReturn(response);
        doThrow(MessageException.class).when(rulesProducer).sendModuleResponseMessage(Matchers.anyObject(), Matchers.anyString());

        rulesEventService.getTicketsAndRulesByMovementsEvent(eventMessage);
        
        ArgumentCaptor<EventMessage> argumentCaptor = ArgumentCaptor.forClass(EventMessage.class);
        verify(errorEvent).fire(argumentCaptor.capture());
        
        EventMessage capturedArgument = argumentCaptor.getValue();
        assertThat(capturedArgument, is(eventMessage));
    }*/
    
}
