package eu.europa.ec.fisheries.uvms.movementrules.service.bean;

import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketStatusType;
import eu.europa.ec.fisheries.uvms.movementrules.model.dto.MovementDetails;
import eu.europa.ec.fisheries.uvms.movementrules.service.RulesTestHelper;
import eu.europa.ec.fisheries.uvms.movementrules.service.TransactionalTests;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.RulesValidator;
import eu.europa.ec.fisheries.uvms.movementrules.service.constants.ServiceConstants;
import eu.europa.ec.fisheries.uvms.movementrules.service.dao.RulesDao;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.PreviousReport;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.RuleSegment;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Ticket;
import org.hamcrest.CoreMatchers;
import org.jboss.arquillian.container.test.api.OperateOnDeployment;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Test;
import org.junit.runner.RunWith;

import javax.inject.Inject;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.*;

@RunWith(Arquillian.class)
public class CustomRulesEvaluatorTest extends TransactionalTests {

    @Inject
    private CustomRulesEvaluator customRulesEvaluator;
    
    @Inject
    private RulesServiceBean rulesService;
    
    @Inject
    private RulesValidator rulesValidator;

    @Inject
    private RulesDao rulesDao;
    
    @Test
    @OperateOnDeployment("normal")
    public void evaluateMovementAndVerifyReportCreated() {
        rulesValidator.updateCustomRules(); // reload/clear rules
        
        List<PreviousReport> previousReportsBefore = rulesService.getPreviousMovementReports();
        
        MovementDetails movementDetails = getMovementDetails();
        customRulesEvaluator.evaluate(movementDetails);

        List<PreviousReport> previousReportsAfter = rulesService.getPreviousMovementReports();
        assertThat(previousReportsAfter.size(), is(previousReportsBefore.size() + 1));
    }

    @Test
    @OperateOnDeployment("normal")
    public void evaluateMovementAndVerifyOpenAssetNotSendingTicketIsClosed() {
        rulesValidator.updateCustomRules(); // reload/clear rules

        MovementDetails movementDetails = getMovementDetails();
        PreviousReport report = new PreviousReport();
        report.setAssetGuid(movementDetails.getAssetGuid());
        rulesService.createAssetNotSendingTicket(ServiceConstants.ASSET_NOT_SENDING_RULE, report);

        Ticket ticket = rulesDao.getTicketByAssetAndRule(movementDetails.getAssetGuid(), ServiceConstants.ASSET_NOT_SENDING_RULE);
        assertNotNull(ticket);
        assertEquals(TicketStatusType.POLL_PENDING, ticket.getStatus());

        customRulesEvaluator.evaluate(movementDetails);

        Ticket closedTicket = rulesDao.getTicketByGuid(ticket.getGuid());
        assertEquals(TicketStatusType.CLOSED, closedTicket.getStatus());

    }
    
    @Test
    @OperateOnDeployment("normal")
    public void evaluateMovementTriggerFlagStateRule() throws Exception {
        String flagState = "SWE";
        MovementDetails movementDetails = getMovementDetails();
        movementDetails.setFlagState(flagState);
        
        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        List<RuleSegment> segments = new ArrayList<>();
        RuleSegment segment = new RuleSegment();
        segment.setCriteria("ASSET");
        segment.setSubCriteria("FLAG_STATE");
        segment.setCondition("EQ");
        segment.setValue(flagState);
        segment.setLogicOperator("NONE");
        segment.setCustomRule(customRule);
        segment.setOrder(0);
        segments.add(segment);
        customRule.setRuleSegmentList(segments);
        rulesService.createCustomRule(customRule, "", "");
        
        customRulesEvaluator.evaluate(movementDetails);
        
        List<Ticket> tickets = rulesService.getTicketsByMovements(Arrays.asList(movementDetails.getMovementGuid()));
        assertThat(tickets.size(), CoreMatchers.is(1));
    }
    
    @Test
    @OperateOnDeployment("normal")
    public void evaluateMovementTriggerAreaRule() throws Exception {
        MovementDetails movementDetails = getMovementDetails();
        // AreaA
        movementDetails.setLongitude(1d);
        movementDetails.setLatitude(1d);
        
        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        List<RuleSegment> segments = new ArrayList<>();
        RuleSegment segment = new RuleSegment();
        segment.setCriteria("AREA");
        segment.setSubCriteria("AREA_CODE");
        segment.setCondition("EQ");
        segment.setValue("AreaA");
        segment.setLogicOperator("NONE");
        segment.setCustomRule(customRule);
        segment.setOrder(0);
        segments.add(segment);
        customRule.setRuleSegmentList(segments);
        rulesService.createCustomRule(customRule, "", "");
        
        customRulesEvaluator.evaluate(movementDetails);
        
        List<Ticket> tickets = rulesService.getTicketsByMovements(Arrays.asList(movementDetails.getMovementGuid()));
        assertThat(tickets.size(), CoreMatchers.is(1));
    }
    
    @Test
    @OperateOnDeployment("normal")
    public void evaluateMovementTriggerAreaEntryRule() throws Exception {
        MovementDetails movementDetails = getMovementDetails();
        // AreaA
        movementDetails.setLongitude(1d);
        movementDetails.setLatitude(1d);
        
        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        List<RuleSegment> segments = new ArrayList<>();
        RuleSegment segment = new RuleSegment();
        segment.setCriteria("AREA");
        segment.setSubCriteria("AREA_CODE_ENT");
        segment.setCondition("EQ");
        segment.setValue("AreaA");
        segment.setLogicOperator("NONE");
        segment.setCustomRule(customRule);
        segment.setOrder(0);
        segments.add(segment);
        customRule.setRuleSegmentList(segments);
        rulesService.createCustomRule(customRule, "", "");
        
        customRulesEvaluator.evaluate(movementDetails);
        
        List<Ticket> tickets = rulesService.getTicketsByMovements(Arrays.asList(movementDetails.getMovementGuid()));
        assertThat(tickets.size(), CoreMatchers.is(1));
    }
    
    @Test
    @OperateOnDeployment("normal")
    public void evaluateMovementTriggerAreaExitRule() throws Exception {
        MovementDetails movementDetails = getMovementDetails();
        // AreaA
        movementDetails.setLongitude(1d);
        movementDetails.setLatitude(1d);
        // AreaB
        movementDetails.setPreviousLatitude(-1d);
        movementDetails.setPreviousLongitude(1d);
        
        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        List<RuleSegment> segments = new ArrayList<>();
        RuleSegment segment = new RuleSegment();
        segment.setCriteria("AREA");
        segment.setSubCriteria("AREA_CODE_EXT");
        segment.setCondition("EQ");
        segment.setValue("AreaB");
        segment.setLogicOperator("NONE");
        segment.setCustomRule(customRule);
        segment.setOrder(0);
        segments.add(segment);
        customRule.setRuleSegmentList(segments);
        rulesService.createCustomRule(customRule, "", "");
        
        customRulesEvaluator.evaluate(movementDetails);
        
        List<Ticket> tickets = rulesService.getTicketsByMovements(Arrays.asList(movementDetails.getMovementGuid()));
        assertThat(tickets.size(), CoreMatchers.is(1));
    }
    
    @Test
    @OperateOnDeployment("normal")
    public void evaluateMovementTriggerAreaEntRuleWithPrevousPosition() throws Exception {
        MovementDetails movementDetails = getMovementDetails();
        // AreaA
        movementDetails.setLongitude(1d);
        movementDetails.setLatitude(1d);
        // AreaB
        movementDetails.setPreviousLatitude(-1d);
        movementDetails.setPreviousLongitude(1d);
        
        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        List<RuleSegment> segments = new ArrayList<>();
        RuleSegment segment = new RuleSegment();
        segment.setCriteria("AREA");
        segment.setSubCriteria("AREA_CODE_ENT");
        segment.setCondition("EQ");
        segment.setValue("AreaA");
        segment.setLogicOperator("NONE");
        segment.setCustomRule(customRule);
        segment.setOrder(0);
        segments.add(segment);
        customRule.setRuleSegmentList(segments);
        rulesService.createCustomRule(customRule, "", "");
        
        customRulesEvaluator.evaluate(movementDetails);
        
        List<Ticket> tickets = rulesService.getTicketsByMovements(Arrays.asList(movementDetails.getMovementGuid()));
        assertThat(tickets.size(), CoreMatchers.is(1));
    }
    
    private MovementDetails getMovementDetails() {
        MovementDetails movementDetails = new MovementDetails();
        movementDetails.setMovementGuid(UUID.randomUUID().toString());
        movementDetails.setLatitude(11d);
        movementDetails.setLongitude(56d);
        movementDetails.setPositionTime(Instant.now());
        movementDetails.setSource("INMARSAT_C");
        movementDetails.setAssetGuid(UUID.randomUUID().toString());
        movementDetails.setFlagState("SWE");
        return movementDetails;
    }
}
