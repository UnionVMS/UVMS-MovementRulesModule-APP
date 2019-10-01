package eu.europa.ec.fisheries.uvms.movementrules.rest.service.arquillian;

import static org.junit.Assert.assertThat;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import java.time.Instant;
import java.util.ArrayList;
import java.util.UUID;
import javax.inject.Inject;
import org.jboss.arquillian.container.test.api.OperateOnDeployment;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Test;
import org.junit.runner.RunWith;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.AvailabilityType;
import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketStatusType;
import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketType;
import eu.europa.ec.fisheries.uvms.movementrules.model.dto.MovementDetails;
import eu.europa.ec.fisheries.uvms.movementrules.rest.service.RulesTestHelper;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.RulesServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.ValidationServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.dao.RulesDao;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.RuleSegment;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.TicketMapper;

@RunWith(Arquillian.class)
public class SSEResourceTest extends BuildRulesRestDeployment {

    private static final String user = "user";

    @Inject
    private RulesServiceBean rulesService;
    
    @Inject
    private ValidationServiceBean validationService;

    @Inject
    private RulesDao rulesDao;
    
    @Test
    @OperateOnDeployment("normal")
    public void sseBroadcastSubscribingToRuleTest() throws Exception {
        
        CustomRule customRule = createCustomRule(user);
        
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        
        MovementDetails movementDetails = getMovementDetails();
            
        try (SSETestClient client = new SSETestClient()) {
            validationService.customRuleTriggered(createdCustomRule.getName(), createdCustomRule.getGuid().toString(), movementDetails, ";");
            
            TicketType ticket = client.getTicket(10000);
            assertThat(ticket.getRuleName(), is(customRule.getName()));
            assertThat(ticket.getMovementGuid(), is(movementDetails.getMovementGuid()));
            assertThat(ticket.getAssetGuid(), is(movementDetails.getAssetGuid()));
        }
        rulesDao.removeCustomRuleAfterTests(customRule);
    }
    
    @Test
    @OperateOnDeployment("normal")
    public void sseBroadcastSubscribingToRuleTwoConnectionsTest() throws Exception {
        
        CustomRule customRule = createCustomRule(user);
        
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        
        MovementDetails movementDetails = getMovementDetails();
            
        try (SSETestClient client = new SSETestClient();
                SSETestClient client2 = new SSETestClient()) {
            validationService.customRuleTriggered(createdCustomRule.getName(), createdCustomRule.getGuid().toString(), movementDetails, ";");
            
            TicketType ticket = client.getTicket(10000);
            assertThat(ticket.getRuleName(), is(customRule.getName()));
            assertThat(ticket.getMovementGuid(), is(movementDetails.getMovementGuid()));
            assertThat(ticket.getAssetGuid(), is(movementDetails.getAssetGuid()));
            
            TicketType ticket2 = client2.getTicket(10000);
            assertThat(ticket2.getRuleName(), is(customRule.getName()));
            assertThat(ticket2.getMovementGuid(), is(movementDetails.getMovementGuid()));
            assertThat(ticket2.getAssetGuid(), is(movementDetails.getAssetGuid()));
        }
        rulesDao.removeCustomRuleAfterTests(customRule);
    }
    
    @Test
    @OperateOnDeployment("normal")
    public void sseBroadcastNotSubscribingToRuleTest() throws Exception {
        String flagstate = "SWE";
        
        CustomRule customRule = createCustomRule(null);
        
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        
        MovementDetails movementDetails = getMovementDetails();
        movementDetails.setFlagState(flagstate);
            
        try (SSETestClient client = new SSETestClient()) {
            validationService.customRuleTriggered(createdCustomRule.getName(), createdCustomRule.getGuid().toString(), movementDetails, ";");
            
            TicketType ticket = client.getTicket(4000);
            assertThat(ticket, is(nullValue()));
        }
        rulesDao.removeCustomRuleAfterTests(customRule);
    }
    
    @Test
    @OperateOnDeployment("normal")
    public void sseBroadcastNotSubscribingToGlobalRuleTest() throws Exception {
        String flagstate = "SWE";
        
        CustomRule customRule = createCustomRule(null);
        
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        createdCustomRule.setAvailability(AvailabilityType.GLOBAL);
        CustomRule updatedRule = rulesService.updateCustomRule(createdCustomRule);
        
        MovementDetails movementDetails = getMovementDetails();
        movementDetails.setFlagState(flagstate);
            
        TicketType ticket;
        try (SSETestClient client = new SSETestClient()) {
            validationService.customRuleTriggered(updatedRule.getName(), updatedRule.getGuid().toString(), movementDetails, ";");
            
            ticket = client.getTicket(10000);
            assertThat(ticket.getRuleName(), is(updatedRule.getName()));
            assertThat(ticket.getMovementGuid(), is(movementDetails.getMovementGuid()));
            assertThat(ticket.getAssetGuid(), is(movementDetails.getAssetGuid()));
        }
        rulesDao.removeTicketAfterTests(TicketMapper.toTicketEntity(ticket));
        rulesDao.removeCustomRuleAfterTests(customRule);
    }
    
    @Test
    @OperateOnDeployment("normal")
    public void sseBroadcastTicketUpdateTest() throws Exception {
        String flagstate = "SWE";
        
        CustomRule customRule = createCustomRule(user);
        
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        
        MovementDetails movementDetails = getMovementDetails();
        movementDetails.setFlagState(flagstate);
            
        try (SSETestClient client = new SSETestClient()) {
            validationService.customRuleTriggered(createdCustomRule.getName(), createdCustomRule.getGuid().toString(), movementDetails, ";");
            
            TicketType ticket = client.getTicket(10000);
            assertThat(ticket.getRuleName(), is(customRule.getName()));
            
            ticket.setStatus(TicketStatusType.CLOSED);
            rulesService.updateTicketStatus(TicketMapper.toTicketEntity(ticket));
            
            TicketType ticketUpdate = client.getTicket(10000);
            
            assertThat(ticketUpdate.getGuid(), is(ticket.getGuid()));
            assertThat(ticketUpdate.getStatus(), is(TicketStatusType.CLOSED));
            assertThat(ticketUpdate.getMovementGuid(), is(movementDetails.getMovementGuid()));
            assertThat(ticketUpdate.getAssetGuid(), is(movementDetails.getAssetGuid()));
        }
        rulesDao.removeCustomRuleAfterTests(customRule);
    }

    private CustomRule createCustomRule(String username) {
        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        if (username != null) {
            customRule.setUpdatedBy(username);
        }
        RuleSegment segment = new RuleSegment();
        segment.setCriteria("ASSET");
        segment.setSubCriteria("FLAG_STATE");
        segment.setCondition("EQ");
        segment.setValue("SWE");
        segment.setLogicOperator("NONE");
        segment.setCustomRule(customRule);
        segment.setOrder(0);
        customRule.getRuleSegmentList().add(segment);
        return customRule;
    }
    
    private MovementDetails getMovementDetails() {
        MovementDetails movementDetails = new MovementDetails();
        movementDetails.setMovementGuid(UUID.randomUUID().toString());
        movementDetails.setConnectId(UUID.randomUUID().toString());
        movementDetails.setLatitude(11d);
        movementDetails.setLongitude(56d);
        movementDetails.setPositionTime(Instant.now());
        movementDetails.setSource("INMARSAT_C");
        movementDetails.setAssetGuid(UUID.randomUUID().toString());
        movementDetails.setFlagState("SWE");
        movementDetails.setAreaTypes(new ArrayList<>());
        return movementDetails;
    }
}
