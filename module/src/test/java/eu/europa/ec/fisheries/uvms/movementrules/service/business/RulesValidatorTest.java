package eu.europa.ec.fisheries.uvms.movementrules.service.business;

import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.*;
import eu.europa.ec.fisheries.uvms.commons.date.DateUtils;
import eu.europa.ec.fisheries.uvms.movementrules.model.dto.MovementDetails;
import eu.europa.ec.fisheries.uvms.movementrules.model.dto.VicinityInfoDTO;
import eu.europa.ec.fisheries.uvms.movementrules.service.RulesTestHelper;
import eu.europa.ec.fisheries.uvms.movementrules.service.TransactionalTests;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.RulesServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.ValidationServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.RuleAction;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.RuleSegment;
import org.jboss.arquillian.container.test.api.OperateOnDeployment;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import javax.inject.Inject;
import javax.transaction.HeuristicMixedException;
import javax.transaction.HeuristicRollbackException;
import javax.transaction.NotSupportedException;
import javax.transaction.RollbackException;
import javax.transaction.SystemException;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.*;

@RunWith(Arquillian.class)
public class RulesValidatorTest extends TransactionalTests {

    @Inject
    RulesValidator rulesValidator;
    
    @Inject
    ValidationServiceBean validationService;
    
    @Inject
    RulesServiceBean rulesService;

    @Before
    public void reloadRules() throws NotSupportedException, SystemException, SecurityException, IllegalStateException, RollbackException, HeuristicMixedException, HeuristicRollbackException {
        rulesService.getRunnableCustomRules().stream().forEach(rule -> rule.setActive(false));
        rulesValidator.updateCustomRules();
        userTransaction.commit();
        userTransaction.begin();
    }

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
        
        assertCustomRuleWasTriggered(createdCustomRule.getGuid().toString(), timestamp);
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
        segment.setValue(DateUtils.dateToEpochMilliseconds(positionTime));
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
        
        assertCustomRuleWasTriggered(createdCustomRule.getGuid().toString(), positionTime);
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
        
        assertCustomRuleWasTriggered(createdCustomRule.getGuid().toString(), timestamp);
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
        
        assertCustomRuleWasTriggered(createdCustomRule.getGuid().toString(), timestamp);
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
        
        assertCustomRuleWasTriggered(createdCustomRule.getGuid().toString(), timestamp);
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
        
        assertCustomRuleWasTriggered(createdCustomRule.getGuid().toString(), timestamp);
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
        
        assertCustomRuleWasTriggered(createdCustomRule.getGuid().toString(), timestamp);
    }

    @Test
    @OperateOnDeployment ("normal")
    public void ruleTriggerPollTest() throws Exception {
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

        RuleAction ruleAction = new RuleAction();
        ruleAction.setCustomRule(customRule);
        ruleAction.setOrder(1);
        ruleAction.setValue("Not Needed");
        ruleAction.setAction(ActionType.MANUAL_POLL.value());
        List<RuleAction> ruleActionList = new ArrayList<>();
        ruleActionList.add(ruleAction);
        ruleActionList.add(RulesTestHelper.createCreateTicketAction(customRule));
        customRule.setRuleActionList(ruleActionList);

        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        long ticketsBefore = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());

        System.setProperty("AssetPollEndpointReached", "False");

        MovementDetails fact = RulesTestHelper.createBasicMovementDetails();
        fact.setFlagState(flagstate);
        rulesValidator.evaluate(fact);

        long ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore + 1));


        assertCustomRuleWasTriggered(createdCustomRule.getGuid().toString(), timestamp);

        assertEquals("True", System.getProperty("AssetPollEndpointReached"));
        System.clearProperty("AssetPollEndpointReached");
    }

    @Test
    @OperateOnDeployment ("normal")
    public void positionTimeB4Test() throws Exception {
        Instant timestamp = getTimestamp();

        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        RuleSegment timeSegment = new RuleSegment();
        timeSegment.setStartOperator("");
        timeSegment.setCriteria(CriteriaType.POSITION.value());
        timeSegment.setSubCriteria(SubCriteriaType.POSITION_REPORT_TIME.value());
        timeSegment.setCondition(ConditionType.LT.value());
        timeSegment.setValue(DateUtils.dateToEpochMilliseconds(timestamp));
        timeSegment.setEndOperator("");
        timeSegment.setLogicOperator(LogicOperatorType.NONE.value());
        timeSegment.setOrder(0);
        timeSegment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(timeSegment);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        long ticketsBefore = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());

        MovementDetails fact = RulesTestHelper.createBasicMovementDetails();
        fact.setPositionTime(timestamp.minusSeconds(60));
        rulesValidator.evaluate(fact);

        long ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore + 1));

        assertCustomRuleWasTriggered(createdCustomRule.getGuid().toString(), timestamp);
    }

    @Test
    @OperateOnDeployment ("normal")
    public void positionTimeAfterTest() throws Exception {
        Instant timestamp = getTimestamp();

        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        RuleSegment timeSegment = new RuleSegment();
        timeSegment.setStartOperator("");
        timeSegment.setCriteria(CriteriaType.POSITION.value());
        timeSegment.setSubCriteria(SubCriteriaType.POSITION_REPORT_TIME.value());
        timeSegment.setCondition(ConditionType.GT.value());
        timeSegment.setValue(DateUtils.dateToEpochMilliseconds(timestamp));
        timeSegment.setEndOperator("");
        timeSegment.setLogicOperator(LogicOperatorType.NONE.value());
        timeSegment.setOrder(0);
        timeSegment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(timeSegment);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        long ticketsBefore = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());

        MovementDetails fact = RulesTestHelper.createBasicMovementDetails();
        fact.setPositionTime(timestamp.plusSeconds(60));
        rulesValidator.evaluate(fact);

        long ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore + 1));

        assertCustomRuleWasTriggered(createdCustomRule.getGuid().toString(), timestamp);
    }


    /*
            VICINITY PART
     */

    @Test
    @OperateOnDeployment ("normal")
    public void triggerVicinityBoatRuleTest() throws Exception {
        Instant timestamp = getTimestamp();
        UUID vicOfID = UUID.randomUUID();

        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        RuleSegment segment = new RuleSegment();
        segment.setStartOperator("");
        segment.setCriteria(CriteriaType.POSITION.value());
        segment.setSubCriteria(SubCriteriaType.VICINITY_OF.value());
        segment.setCondition(ConditionType.EQ.value());
        segment.setValue(vicOfID.toString());
        segment.setLogicOperator(LogicOperatorType.NONE.value());
        segment.setEndOperator("");
        segment.setOrder(0);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        long ticketsBefore = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());

        MovementDetails fact = RulesTestHelper.createBasicMovementDetails();
        List<VicinityInfoDTO> vicList = new ArrayList<>();
        VicinityInfoDTO vic = new VicinityInfoDTO();
        vic.setAsset(vicOfID.toString());
        vic.setDistance(500);
        vicList.add(vic);
        fact.setVicinityOf(vicList);
        rulesValidator.evaluate(fact);

        long ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore + 1));

        assertCustomRuleWasTriggered(createdCustomRule.getGuid().toString(), timestamp);

        //dont trigger if different

        vic.setAsset(UUID.randomUUID().toString());

        rulesValidator.evaluate(fact);

        ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore + 1));

        assertCustomRuleWasTriggered(createdCustomRule.getGuid().toString(), timestamp);
    }

    @Test
    @OperateOnDeployment ("normal")
    public void triggerVicinityDistanceRuleTest() throws Exception {
        Instant timestamp = getTimestamp();
        UUID vicOfID = UUID.randomUUID();

        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        RuleSegment segment = new RuleSegment();
        segment.setStartOperator("");
        segment.setCriteria(CriteriaType.POSITION.value());
        segment.setSubCriteria(SubCriteriaType.VICINITY_DISTANCE_OF.value());
        segment.setCondition(ConditionType.LT.value());
        segment.setValue("500");
        segment.setLogicOperator(LogicOperatorType.NONE.value());
        segment.setEndOperator("");
        segment.setOrder(0);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        long ticketsBefore = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());

        MovementDetails fact = RulesTestHelper.createBasicMovementDetails();
        List<VicinityInfoDTO> vicList = new ArrayList<>();
        VicinityInfoDTO vic = new VicinityInfoDTO();
        vic.setAsset(vicOfID.toString());
        vic.setDistance(400);
        vicList.add(vic);
        fact.setVicinityOf(vicList);
        rulesValidator.evaluate(fact);

        long ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore + 1));

        assertCustomRuleWasTriggered(createdCustomRule.getGuid().toString(), timestamp);

        //dont trigger if longer
        vic.setDistance(600);

        rulesValidator.evaluate(fact);

        ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore + 1));

        assertCustomRuleWasTriggered(createdCustomRule.getGuid().toString(), timestamp);
    }


    @Test
    @OperateOnDeployment ("normal")
    public void triggerVicinityBoatRuleMoreComplexORRuleTest() throws Exception {
        Instant timestamp = getTimestamp();
        UUID vicOfID = UUID.randomUUID();

        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        RuleSegment segment = new RuleSegment();
        segment.setStartOperator("");
        segment.setCriteria(CriteriaType.POSITION.value());
        segment.setSubCriteria(SubCriteriaType.VICINITY_OF.value());
        segment.setCondition(ConditionType.EQ.value());
        segment.setValue(vicOfID.toString());
        segment.setLogicOperator(LogicOperatorType.AND.value());
        segment.setEndOperator("");
        segment.setOrder(0);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);

        segment = new RuleSegment();
        segment.setStartOperator("(");
        segment.setCriteria(CriteriaType.POSITION.value());
        segment.setSubCriteria(SubCriteriaType.VICINITY_OF.value());
        segment.setCondition(ConditionType.NE.value());
        segment.setValue(UUID.randomUUID().toString());
        segment.setLogicOperator(LogicOperatorType.OR.value());
        segment.setEndOperator("");
        segment.setOrder(1);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);

        segment = new RuleSegment();
        segment.setStartOperator("");
        segment.setCriteria(CriteriaType.POSITION.value());
        segment.setSubCriteria(SubCriteriaType.ALTITUDE.value());
        segment.setCondition(ConditionType.GT.value());
        segment.setValue("50");
        segment.setLogicOperator(LogicOperatorType.AND.value());
        segment.setEndOperator(")");
        segment.setOrder(2);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);

        segment = new RuleSegment();
        segment.setStartOperator("");
        segment.setCriteria(CriteriaType.POSITION.value());
        segment.setSubCriteria(SubCriteriaType.VICINITY_DISTANCE_OF.value());
        segment.setCondition(ConditionType.LE.value());
        segment.setValue("500");
        segment.setLogicOperator(LogicOperatorType.NONE.value());
        segment.setEndOperator("");
        segment.setOrder(3);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        long ticketsBefore = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());

        MovementDetails fact = RulesTestHelper.createBasicMovementDetails();
        fact.setAltitude(9001d);
        List<VicinityInfoDTO> vicList = new ArrayList<>();
        VicinityInfoDTO vic = new VicinityInfoDTO();
        vic.setAsset(vicOfID.toString());
        vic.setDistance(500);
        vicList.add(vic);
        fact.setVicinityOf(vicList);
        rulesValidator.evaluate(fact);

        long ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore + 2));    //drools handles 'OR' in a really wierd way, internally creating separate rules for every 'OR' condition, thus giving more activations. For more info see: https://stackoverflow.com/questions/46858200/drools-aftermatchfiringevent-triggering-multiple-times-for-the-same-rule-with-o

        assertCustomRuleWasTriggered(createdCustomRule.getGuid().toString(), timestamp);
    }

    @Test
    @OperateOnDeployment ("normal")
    public void triggerVicinityBoatRuleMoreComplexANDRuleTest() throws Exception {
        Instant timestamp = getTimestamp();
        UUID vicOfID = UUID.randomUUID();

        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        RuleSegment segment = new RuleSegment();
        segment.setStartOperator("");
        segment.setCriteria(CriteriaType.POSITION.value());
        segment.setSubCriteria(SubCriteriaType.VICINITY_DISTANCE_OF.value());
        segment.setCondition(ConditionType.GT.value());
        segment.setValue("150");
        segment.setLogicOperator(LogicOperatorType.AND.value());
        segment.setEndOperator("");
        segment.setOrder(0);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);

        segment = new RuleSegment();
        segment.setStartOperator("((");
        segment.setCriteria(CriteriaType.POSITION.value());
        segment.setSubCriteria(SubCriteriaType.FLAG_STATE.value());
        segment.setCondition(ConditionType.EQ.value());
        segment.setValue("SWE");
        segment.setLogicOperator(LogicOperatorType.AND.value());
        segment.setEndOperator("");
        segment.setOrder(1);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);

        segment = new RuleSegment();
        segment.setStartOperator("");
        segment.setCriteria(CriteriaType.POSITION.value());
        segment.setSubCriteria(SubCriteriaType.ALTITUDE.value());
        segment.setCondition(ConditionType.GT.value());
        segment.setValue("50");
        segment.setLogicOperator(LogicOperatorType.AND.value());
        segment.setEndOperator(")");
        segment.setOrder(2);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);

        segment = new RuleSegment();
        segment.setStartOperator("");
        segment.setCriteria(CriteriaType.POSITION.value());
        segment.setSubCriteria(SubCriteriaType.VICINITY_OF.value());
        segment.setCondition(ConditionType.EQ.value());
        segment.setValue(vicOfID.toString());
        segment.setLogicOperator(LogicOperatorType.NONE.value());
        segment.setEndOperator(")");
        segment.setOrder(3);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        long ticketsBefore = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());

        MovementDetails fact = RulesTestHelper.createBasicMovementDetails();
        List<VicinityInfoDTO> vicList = new ArrayList<>();
        VicinityInfoDTO vic = new VicinityInfoDTO();
        vic.setAsset(UUID.randomUUID().toString());
        vic.setDistance(50);
        vicList.add(vic);
        fact.setVicinityOf(vicList);
        fact.setAltitude(0d);
        fact.setFlagState("DNK");
        rulesValidator.evaluate(fact);

        //Start checking if it triggered
        long ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore));

        fact.setFlagState("SWE");
        rulesValidator.evaluate(fact);

        ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore));

        vic.setAsset(vicOfID.toString());
        rulesValidator.evaluate(fact);

        ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore));

        fact.setAltitude(51d);
        rulesValidator.evaluate(fact);

        ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore));

        vic.setDistance(200);
        rulesValidator.evaluate(fact);

        ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore + 1));

        assertCustomRuleWasTriggered(createdCustomRule.getGuid().toString(), timestamp);
    }


    @Test
    @OperateOnDeployment ("normal")
    public void triggerVicinityBoatRuleManyParenthesisRuleTest() throws Exception {
        Instant timestamp = getTimestamp();
        UUID vicOfID = UUID.randomUUID();

        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        RuleSegment segment = new RuleSegment();
        segment.setStartOperator("(((");
        segment.setCriteria(CriteriaType.POSITION.value());
        segment.setSubCriteria(SubCriteriaType.VICINITY_DISTANCE_OF.value());
        segment.setCondition(ConditionType.GT.value());
        segment.setValue("150");
        segment.setLogicOperator(LogicOperatorType.AND.value());
        segment.setEndOperator("");
        segment.setOrder(0);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);

        segment = new RuleSegment();
        segment.setStartOperator("");
        segment.setCriteria(CriteriaType.POSITION.value());
        segment.setSubCriteria(SubCriteriaType.FLAG_STATE.value());
        segment.setCondition(ConditionType.EQ.value());
        segment.setValue("SWE");
        segment.setLogicOperator(LogicOperatorType.AND.value());
        segment.setEndOperator(")");
        segment.setOrder(1);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);

        segment = new RuleSegment();
        segment.setStartOperator("");
        segment.setCriteria(CriteriaType.POSITION.value());
        segment.setSubCriteria(SubCriteriaType.ALTITUDE.value());
        segment.setCondition(ConditionType.GT.value());
        segment.setValue("50");
        segment.setLogicOperator(LogicOperatorType.AND.value());
        segment.setEndOperator(")");
        segment.setOrder(2);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);

        segment = new RuleSegment();
        segment.setStartOperator("");
        segment.setCriteria(CriteriaType.POSITION.value());
        segment.setSubCriteria(SubCriteriaType.VICINITY_OF.value());
        segment.setCondition(ConditionType.EQ.value());
        segment.setValue(vicOfID.toString());
        segment.setLogicOperator(LogicOperatorType.NONE.value());
        segment.setEndOperator(")");
        segment.setOrder(3);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        long ticketsBefore = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());

        MovementDetails fact = RulesTestHelper.createBasicMovementDetails();
        List<VicinityInfoDTO> vicList = new ArrayList<>();
        VicinityInfoDTO vic = new VicinityInfoDTO();
        vic.setAsset(vicOfID.toString());
        vic.setDistance(500);
        vicList.add(vic);
        fact.setVicinityOf(vicList);
        fact.setAltitude(500d);
        fact.setFlagState("SWE");
        rulesValidator.evaluate(fact);

        //Start checking if it triggered

        long ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore + 1));

        assertCustomRuleWasTriggered(createdCustomRule.getGuid().toString(), timestamp);
    }

    @Test
    @OperateOnDeployment ("normal")
    public void triggerVicinityBoatRuleManyParenthesisInFrontOfVicinityRuleTest() throws Exception {
        Instant timestamp = getTimestamp();
        UUID vicOfID = UUID.randomUUID();

        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        RuleSegment segment = new RuleSegment();
        segment.setStartOperator("(((");
        segment.setCriteria(CriteriaType.POSITION.value());
        segment.setSubCriteria(SubCriteriaType.VICINITY_OF.value());
        segment.setCondition(ConditionType.GT.value());
        segment.setValue(vicOfID.toString());
        segment.setLogicOperator(LogicOperatorType.AND.value());
        segment.setEndOperator("");
        segment.setOrder(0);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);

        segment = new RuleSegment();
        segment.setStartOperator("");
        segment.setCriteria(CriteriaType.POSITION.value());
        segment.setSubCriteria(SubCriteriaType.ALTITUDE.value());
        segment.setCondition(ConditionType.GT.value());
        segment.setValue("50");
        segment.setLogicOperator(LogicOperatorType.AND.value());
        segment.setEndOperator(")");
        segment.setOrder(2);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);

        segment = new RuleSegment();
        segment.setStartOperator("");
        segment.setCriteria(CriteriaType.POSITION.value());
        segment.setSubCriteria(SubCriteriaType.VICINITY_OF.value());
        segment.setCondition(ConditionType.NE.value());
        segment.setValue(UUID.randomUUID().toString());
        segment.setLogicOperator(LogicOperatorType.NONE.value());
        segment.setEndOperator("))");
        segment.setOrder(3);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        long ticketsBefore = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());

        MovementDetails fact = RulesTestHelper.createBasicMovementDetails();
        List<VicinityInfoDTO> vicList = new ArrayList<>();
        VicinityInfoDTO vic = new VicinityInfoDTO();
        vic.setAsset(vicOfID.toString());
        vic.setDistance(500);
        vicList.add(vic);
        fact.setVicinityOf(vicList);
        fact.setAltitude(500d);
        fact.setFlagState("SWE");
        rulesValidator.evaluate(fact);

        //Start checking if it triggered

        long ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore + 1));

        assertCustomRuleWasTriggered(createdCustomRule.getGuid().toString(), timestamp);
    }


    @Test
    @OperateOnDeployment ("normal")
    public void triggerVicinityBoatRuleSeveralBoatsInProximityRuleTest() throws Exception {
        Instant timestamp = getTimestamp();
        UUID vicOfID = UUID.randomUUID();

        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        RuleSegment segment = new RuleSegment();
        segment.setStartOperator("(");
        segment.setCriteria(CriteriaType.POSITION.value());
        segment.setSubCriteria(SubCriteriaType.VICINITY_OF.value());
        segment.setCondition(ConditionType.EQ.value());
        segment.setValue(vicOfID.toString());
        segment.setLogicOperator(LogicOperatorType.AND.value());
        segment.setEndOperator("");
        segment.setOrder(0);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);

        segment = new RuleSegment();
        segment.setStartOperator("");
        segment.setCriteria(CriteriaType.POSITION.value());
        segment.setSubCriteria(SubCriteriaType.VICINITY_DISTANCE_OF.value());
        segment.setCondition(ConditionType.LE.value());
        segment.setValue("300");
        segment.setLogicOperator(LogicOperatorType.NONE.value());
        segment.setEndOperator(")");
        segment.setOrder(3);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        long ticketsBefore = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());

        MovementDetails fact = RulesTestHelper.createBasicMovementDetails();
        List<VicinityInfoDTO> vicList = new ArrayList<>();
        VicinityInfoDTO vic1 = new VicinityInfoDTO();
        vic1.setAsset(vicOfID.toString());
        vic1.setDistance(600);
        vicList.add(vic1);

        VicinityInfoDTO vic2 = new VicinityInfoDTO();
        vic2.setAsset(UUID.randomUUID().toString());
        vic2.setDistance(50);
        vicList.add(vic2);
        fact.setVicinityOf(vicList);
        rulesValidator.evaluate(fact);

        //Checking if it triggered

        long ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore));

        vic1.setDistance(10.5);
        rulesValidator.evaluate(fact);

        ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore + 1));
    }
    
    private void assertCustomRuleWasTriggered(String ruleGuid, Instant fromDate) throws Exception {
        CustomRule customRule = rulesService.getCustomRuleByGuid(UUID.fromString(ruleGuid));
        assertThat(customRule.getLastTriggered(), is(notNullValue()));
        assertTrue(customRule.getLastTriggered().isAfter(fromDate)
                || customRule.getLastTriggered().equals(fromDate));
    }
    
    private Instant getTimestamp() {
        return Instant.now().truncatedTo(ChronoUnit.SECONDS);
    }
}
