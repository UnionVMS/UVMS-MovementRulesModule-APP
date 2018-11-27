/*
﻿Developed with the contribution of the European Commission - Directorate General for Maritime Affairs and Fisheries
© European Union, 2015-2016.

This file is part of the Integrated Fisheries Data Management (IFDM) Suite. The IFDM Suite is free software: you can
redistribute it and/or modify it under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or any later version. The IFDM Suite is distributed in
the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a
copy of the GNU General Public License along with the IFDM Suite. If not, see <http://www.gnu.org/licenses/>.
 */
package eu.europa.ec.fisheries.uvms.movementrules.service;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Random;
import java.util.UUID;
import eu.europa.ec.fisheries.schema.movementrules.asset.v1.AssetType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.ActionType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.AvailabilityType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.ConditionType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.CriteriaType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.LogicOperatorType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.SubCriteriaType;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.MovementActivityTypeType;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.MovementSourceType;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.AlarmQuery;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.CustomRuleQuery;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.ListPagination;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.TicketQuery;
import eu.europa.ec.fisheries.uvms.movementrules.model.dto.MovementDetails;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.RuleAction;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.RuleSegment;

public class RulesTestHelper {

    public static CustomRule createBasicCustomRule() {
        CustomRule customRule = new CustomRule();

        customRule.setName("Test rule" + " (" + System.currentTimeMillis() + ")");
        customRule.setAvailability(AvailabilityType.PRIVATE);
        customRule.setUpdatedBy("vms_admin_com");
        customRule.setActive(true);
        customRule.setArchived(false);
        return customRule;
    }
    
    public static CustomRule createCompleteCustomRule() {
        CustomRule customRule = new CustomRule();

        customRule.setName("Test rule" + " (" + System.currentTimeMillis() + ")");
        customRule.setAvailability(AvailabilityType.PRIVATE);
        customRule.setUpdatedBy("vms_admin_com");
        customRule.setActive(true);
        customRule.setArchived(false);

        RuleSegment flagStateRule = new RuleSegment();
        flagStateRule.setStartOperator("(");
        flagStateRule.setCriteria(CriteriaType.ASSET.value());
        flagStateRule.setSubCriteria(SubCriteriaType.FLAG_STATE.value());
        flagStateRule.setCondition(ConditionType.EQ.value());
        flagStateRule.setValue("SWE");
        flagStateRule.setEndOperator(")");
        flagStateRule.setLogicOperator(LogicOperatorType.AND.value());
        flagStateRule.setOrder(0);
        flagStateRule.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(flagStateRule);

        RuleSegment areaRule = new RuleSegment();
        areaRule.setStartOperator("(");
        areaRule.setCriteria(CriteriaType.AREA.value());
        areaRule.setSubCriteria(SubCriteriaType.AREA_CODE.value());
        areaRule.setCondition(ConditionType.EQ.value());
        areaRule.setValue("DNK");
        areaRule.setEndOperator(")");
        areaRule.setLogicOperator(LogicOperatorType.NONE.value());
        areaRule.setOrder(1);
        areaRule.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(areaRule);

        RuleAction action = new RuleAction();
        action.setAction(ActionType.SEND_TO_FLUX.value());
        action.setValue("DNK");
        action.setOrder(0);
        action.setCustomRule(customRule);
        customRule.getRuleActionList().add(action);

        return customRule;
    }
    
    public static CustomRuleQuery createBasicCustomRuleQuery() {
        CustomRuleQuery query = new CustomRuleQuery();
        query.setDynamic(true);
        ListPagination pagination = new ListPagination();
        pagination.setPage(1);
        pagination.setListSize(100);
        query.setPagination(pagination);
        return query;
    }
    
    public static MovementDetails createBasicMovementDetails() {
        MovementDetails movementDetails = new MovementDetails();
        movementDetails.setLatitude(56d);
        movementDetails.setLongitude(11d);
        movementDetails.setPositionTime(Instant.now());
        movementDetails.setAssetGuid(UUID.randomUUID().toString());
        movementDetails.setAssetType(AssetType.VESSEL.value());
        movementDetails.setFlagState("SWE");
        movementDetails.setClosestCountryCode("SWE");
        movementDetails.setClosestPortCode("GBG");
        movementDetails.setAreaCodes(Arrays.asList("DNK"));
        movementDetails.setActivityMessageType(MovementActivityTypeType.COE.value());
        movementDetails.setActivityMessageId("messageId");
        movementDetails.setSource(MovementSourceType.NAF.value());
        movementDetails.setAreaCodes(new ArrayList<>());
        movementDetails.setAreaTypes(new ArrayList<>());
        movementDetails.setEntAreaCodes(new ArrayList<>());
        movementDetails.setExtAreaCodes(new ArrayList<>());
        return movementDetails;
    }
    
    public static AlarmQuery getBasicAlarmQuery() {
        AlarmQuery query = new AlarmQuery();
        query.setDynamic(true);
        ListPagination pagination = new ListPagination();
        pagination.setPage(1);
        pagination.setListSize(100);
        query.setPagination(pagination);
        return query;
    }


    public static TicketQuery getBasicTicketQuery() {
        TicketQuery query = new TicketQuery();
        ListPagination pagination = new ListPagination();
        pagination.setPage(1);
        pagination.setListSize(100);
        query.setPagination(pagination);
        return query;
    }
    
    public static String getRandomIntegers(int length) {
        return new Random()
                .ints(0,9)
                .mapToObj(i -> String.valueOf(i))
                .limit(length)
                .collect(StringBuilder::new, StringBuilder::append, StringBuilder::append)
                .toString();
    }
}
