package eu.europa.ec.fisheries.uvms.movementrules.service.business;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;
import java.util.UUID;
import javax.inject.Inject;
import org.jboss.arquillian.container.test.api.OperateOnDeployment;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Test;
import org.junit.runner.RunWith;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.ConditionType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.CriteriaType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.LogicOperatorType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.SubCriteriaType;
import eu.europa.ec.fisheries.uvms.movementrules.model.dto.MovementDetails;
import eu.europa.ec.fisheries.uvms.movementrules.service.RulesTestHelper;
import eu.europa.ec.fisheries.uvms.movementrules.service.TransactionalTests;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.RulesServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.ValidationServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.RuleSegment;

@RunWith(Arquillian.class)
public class RulesValidatorTest extends TransactionalTests {

    @Inject
    RulesValidator rulesValidator;
    
    @Inject
    ValidationServiceBean validationService;
    
    @Inject
    RulesServiceBean rulesService;



    /*
     * Custom Rules
     */
    
    @Test
    @OperateOnDeployment ("normal")
    public void evaluateMovementFactTriggerFlagStateRuleTest() throws Exception {
        Instant timestamp = getTimestamp();
        String flagstate = "SWE";

        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        RuleSegment segment = new RuleSegment();
        segment.setStartOperator("");
        segment.setCriteria(CriteriaType.ASSET.value());
        segment.setSubCriteria(SubCriteriaType.FLAG_STATE.value());
        segment.setCondition(ConditionType.EQ.value());
        segment.setValue(flagstate);
        segment.setLogicOperator(LogicOperatorType.NONE.value());
        segment.setEndOperator("");
        segment.setOrder(0);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        
        long ticketsBefore = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        
        MovementDetails fact = RulesTestHelper.createBasicMovementDetails();
        fact.setFlagState(flagstate);
        rulesValidator.evaluate(fact);
        
        long ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore + 1));
        
        assertCustomRuleWasTriggered(createdCustomRule.getGuid(), timestamp);
    }
    
    @Test
    @OperateOnDeployment ("normal")
    public void evaluateMovementFactDontTriggerFlagStateRuleTest() throws Exception {
        Instant timestamp = getTimestamp();
        String flagstate = "SWE";

        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        RuleSegment segment = new RuleSegment();
        segment.setCriteria(CriteriaType.ASSET.value());
        segment.setSubCriteria(SubCriteriaType.FLAG_STATE.value());
        segment.setCondition(ConditionType.EQ.value());
        segment.setValue(flagstate);
        segment.setLogicOperator(LogicOperatorType.NONE.value());
        segment.setOrder(0);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        
        long ticketsBefore = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        
        MovementDetails fact = RulesTestHelper.createBasicMovementDetails();
        fact.setFlagState("TEST");
        rulesValidator.evaluate(fact);
        
        long ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore));
    }

    @Test
    @OperateOnDeployment ("normal")
    public void evaluateMovementFactTriggerPositionTimeRuleTest() throws Exception {
        Instant positionTime = getTimestamp();

        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        RuleSegment segment = new RuleSegment();
        segment.setStartOperator("");
        segment.setCriteria(CriteriaType.POSITION.value());
        segment.setSubCriteria(SubCriteriaType.POSITION_REPORT_TIME.value());
        segment.setCondition(ConditionType.EQ.value());
        segment.setValue(MRDateUtils.dateToString(positionTime));
        segment.setEndOperator("");
        segment.setLogicOperator(LogicOperatorType.NONE.value());
        segment.setOrder(0);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        
        long ticketsBefore = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        
        MovementDetails fact = RulesTestHelper.createBasicMovementDetails();
        fact.setPositionTime(positionTime);
        rulesValidator.evaluate(fact);
        
        long ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore + 1));
        
        assertCustomRuleWasTriggered(createdCustomRule.getGuid(), positionTime);
    }
    
    @Test
    @OperateOnDeployment ("normal")
    public void evaluateMovementFactTriggerAreaRuleTest() throws Exception {
        Instant timestamp = getTimestamp();
        String areaCode = "SWE";

        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        RuleSegment segment = new RuleSegment();
        segment.setStartOperator("");
        segment.setCriteria(CriteriaType.AREA.value());
        segment.setSubCriteria(SubCriteriaType.AREA_CODE.value());
        segment.setCondition(ConditionType.EQ.value());
        segment.setValue(areaCode);
        segment.setEndOperator("");
        segment.setLogicOperator(LogicOperatorType.NONE.value());
        segment.setOrder(0);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        
        long ticketsBefore = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        
        MovementDetails fact = RulesTestHelper.createBasicMovementDetails();
        fact.getAreaCodes().add(areaCode);
        rulesValidator.evaluate(fact);
        
        long ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore + 1));
        
        assertCustomRuleWasTriggered(createdCustomRule.getGuid(), timestamp);
    }
    
    @Test
    @OperateOnDeployment ("normal")
    public void evaluateMovementFactTriggerAreaEntryRuleTest() throws Exception {
        Instant timestamp = getTimestamp();
        String areaCode = "SWE";

        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        RuleSegment segment = new RuleSegment();
        segment.setStartOperator("");
        segment.setCriteria(CriteriaType.AREA.value());
        segment.setSubCriteria(SubCriteriaType.AREA_CODE_ENT.value());
        segment.setCondition(ConditionType.EQ.value());
        segment.setValue(areaCode);
        segment.setEndOperator("");
        segment.setLogicOperator(LogicOperatorType.NONE.value());
        segment.setOrder(0);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        
        long ticketsBefore = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        
        MovementDetails fact = RulesTestHelper.createBasicMovementDetails();
        fact.getEntAreaCodes().add(areaCode);
        rulesValidator.evaluate(fact);
        
        long ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore + 1));
        
        assertCustomRuleWasTriggered(createdCustomRule.getGuid(), timestamp);
    }
    
    @Test
    @OperateOnDeployment ("normal")
    public void evaluateMovementFactTriggerAreaExitRuleTest() throws Exception {
        Instant timestamp = getTimestamp();
        String areaCode = "SWE";

        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        RuleSegment segment = new RuleSegment();
        segment.setStartOperator("");
        segment.setCriteria(CriteriaType.AREA.value());
        segment.setSubCriteria(SubCriteriaType.AREA_CODE_EXT.value());
        segment.setCondition(ConditionType.EQ.value());
        segment.setValue(areaCode);
        segment.setEndOperator("");
        segment.setLogicOperator(LogicOperatorType.NONE.value());
        segment.setOrder(0);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        
        long ticketsBefore = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        
        MovementDetails fact = RulesTestHelper.createBasicMovementDetails();
        fact.getExtAreaCodes().add(areaCode);
        rulesValidator.evaluate(fact);
        
        long ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore + 1));
        
        assertCustomRuleWasTriggered(createdCustomRule.getGuid(), timestamp);
    }
    
    @Test
    @OperateOnDeployment ("normal")
    public void evaluateMovementFactTriggerMTSerialNumberRuleTest() throws Exception {
        Instant timestamp = getTimestamp();
        String serialNumber = UUID.randomUUID().toString();

        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        RuleSegment segment = new RuleSegment();
        segment.setStartOperator("");
        segment.setCriteria(CriteriaType.MOBILE_TERMINAL.value());
        segment.setSubCriteria(SubCriteriaType.MT_SERIAL_NO.value());
        segment.setCondition(ConditionType.EQ.value());
        segment.setValue(serialNumber);
        segment.setEndOperator("");
        segment.setLogicOperator(LogicOperatorType.NONE.value());
        segment.setOrder(0);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        
        long ticketsBefore = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        
        MovementDetails fact = RulesTestHelper.createBasicMovementDetails();
        fact.setMobileTerminalSerialNumber(serialNumber);
        rulesValidator.evaluate(fact);
        
        long ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore + 1));
        
        assertCustomRuleWasTriggered(createdCustomRule.getGuid(), timestamp);
    }
    
    @Test
    @OperateOnDeployment ("normal")
    public void evaluateMovementFactTriggerFlagStateAndAreaRuleTest() throws Exception {
        Instant timestamp = getTimestamp();
        String flagstate = "SWE";
        String area = "SWE";

        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        RuleSegment flagstateSegment = new RuleSegment();
        flagstateSegment.setStartOperator("");
        flagstateSegment.setCriteria(CriteriaType.ASSET.value());
        flagstateSegment.setSubCriteria(SubCriteriaType.FLAG_STATE.value());
        flagstateSegment.setCondition(ConditionType.EQ.value());
        flagstateSegment.setValue(flagstate);
        flagstateSegment.setEndOperator("");
        flagstateSegment.setLogicOperator(LogicOperatorType.AND.value());
        flagstateSegment.setOrder(0);
        flagstateSegment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(flagstateSegment);
        RuleSegment areaSegment = new RuleSegment();
        areaSegment.setStartOperator("");
        areaSegment.setCriteria(CriteriaType.AREA.value());
        areaSegment.setSubCriteria(SubCriteriaType.AREA_CODE.value());
        areaSegment.setCondition(ConditionType.EQ.value());
        areaSegment.setValue(area);
        areaSegment.setEndOperator("");
        areaSegment.setLogicOperator(LogicOperatorType.NONE.value());
        areaSegment.setOrder(1);
        areaSegment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(areaSegment);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        
        long ticketsBefore = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        
        MovementDetails fact = RulesTestHelper.createBasicMovementDetails();
        fact.setFlagState(flagstate);
        fact.setAreaCodes(Arrays.asList(area));
        rulesValidator.evaluate(fact);
        
        long ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore + 1));
        
        assertCustomRuleWasTriggered(createdCustomRule.getGuid(), timestamp);
    }
    
    private void assertCustomRuleWasTriggered(String ruleGuid, Instant fromDate) throws Exception {
        CustomRule customRule = rulesService.getCustomRuleByGuid(ruleGuid);
        assertThat(customRule.getTriggered(), is(notNullValue()));
        assertTrue(customRule.getTriggered().isAfter(fromDate)
                || customRule.getTriggered().equals(fromDate));
    }
    
    private Instant getTimestamp() {
        return Instant.now().truncatedTo(ChronoUnit.SECONDS);
    }
}
