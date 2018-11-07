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
package eu.europa.ec.fisheries.uvms.movementrules.rest.service;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import eu.europa.ec.fisheries.schema.exchange.plugin.types.v1.PluginType;
import eu.europa.ec.fisheries.schema.movement.v1.MovementType;
import eu.europa.ec.fisheries.schema.movementrules.asset.v1.AssetId;
import eu.europa.ec.fisheries.schema.movementrules.asset.v1.AssetIdList;
import eu.europa.ec.fisheries.schema.movementrules.asset.v1.AssetIdType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.*;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.RawMovementType;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.AlarmQuery;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.CustomRuleQuery;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.ListPagination;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.TicketQuery;
import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketStatusType;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.MovementFact;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.RawMovementFact;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Ticket;

import java.util.Date;
import java.util.Random;
import java.util.UUID;

public class RulesTestHelper {

    public static CustomRule createBasicCustomRule() {
        CustomRule customRule = new CustomRule();
        customRule.setName("Test Rule " + System.currentTimeMillis());
        customRule.setAvailability(AvailabilityType.PRIVATE.value());
        customRule.setUpdatedBy("Test User");
        customRule.setActive(true);
        customRule.setArchived(false);
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
    
    public static RawMovementFact createBasicRawMovementFact() {
        RawMovementFact rawMovementFact = new RawMovementFact();
        RawMovementType rawMovementType = new RawMovementType();
        rawMovementType.setGuid(UUID.randomUUID().toString());
        AssetId assetId = new AssetId();
        AssetIdList assetIdList = new AssetIdList();
        assetIdList.setIdType(AssetIdType.CFR);
        assetIdList.setValue("CFR" + getRandomIntegers(5));
        assetId.getAssetIdList().add(assetIdList);
        rawMovementType.setAssetId(assetId);
        rawMovementFact.setRawMovementType(rawMovementType);
        rawMovementFact.setPluginType(PluginType.NAF.value());
        rawMovementFact.setLatitude(1d);
        rawMovementFact.setLongitude(1d);
        rawMovementFact.setAssetGuid(UUID.randomUUID().toString());
        rawMovementFact.setComChannelType("ComChannelType");
        rawMovementFact.setPositionTime(new Date());
        rawMovementFact.setMobileTerminalMemberNumber(getRandomIntegers(5));
        rawMovementFact.setMobileTerminalDnid(getRandomIntegers(5));
        rawMovementFact.setIrcs(getRandomIntegers(7));
        rawMovementFact.setCfr(getRandomIntegers(7));
        return rawMovementFact;
    }
    
    public static MovementFact createBasicMovementFact() {
        MovementFact movementFact = new MovementFact();
        MovementType movement = new MovementType();
        movementFact.setMovementMovement(movement);
        return movementFact;
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

    public static CustomRuleType getCompleteNewCustomRule(){
        CustomRuleType customRule = new CustomRuleType();

        customRule.setName("Flag SWE && area DNK => Send to DNK" + " (" + System.currentTimeMillis() + ")");
        customRule.setAvailability(AvailabilityType.PRIVATE);
        customRule.setUpdatedBy("vms_admin_com");
        customRule.setActive(true);
        customRule.setArchived(false);

        // If flagstate = SWE
        CustomRuleSegmentType flagStateRule = new CustomRuleSegmentType();
        flagStateRule.setStartOperator("(");
        flagStateRule.setCriteria(CriteriaType.ASSET);
        flagStateRule.setSubCriteria(SubCriteriaType.FLAG_STATE);
        flagStateRule.setCondition(ConditionType.EQ);
        flagStateRule.setValue("SWE");
        flagStateRule.setEndOperator(")");
        flagStateRule.setLogicBoolOperator(LogicOperatorType.AND);
        flagStateRule.setOrder("0");
        customRule.getDefinitions().add(flagStateRule);

        // and area = DNK
        CustomRuleSegmentType areaRule = new CustomRuleSegmentType();
        areaRule.setStartOperator("(");
        areaRule.setCriteria(CriteriaType.AREA);
        areaRule.setSubCriteria(SubCriteriaType.AREA_CODE);
        areaRule.setCondition(ConditionType.EQ);
        areaRule.setValue("DNK");
        areaRule.setEndOperator(")");
        areaRule.setLogicBoolOperator(LogicOperatorType.NONE);
        areaRule.setOrder("1");
        customRule.getDefinitions().add(areaRule);

        // then send to FLUX DNK
        CustomRuleActionType action = new CustomRuleActionType();
        action.setAction(ActionType.SEND_TO_FLUX);
        action.setValue("FLUX DNK");
        action.setOrder("0");

        customRule.getActions().add(action);

        return customRule;
    }


    public static Ticket getCompleteTicket() {
        Ticket ticket = new Ticket();
        ticket.setUpdated(new Date());
        ticket.setCreatedDate(new Date());
        ticket.setStatus(TicketStatusType.OPEN);
        ticket.setUpdatedBy("test user");
        ticket.setRuleGuid("tmp rule guid");
        ticket.setAssetGuid("tmp asset guid");
        ticket.setMovementGuid("tmp movement guid");
        ticket.setRuleName("tmp rule name");
        ticket.setTicketCount(1L);
        ticket.setChannelGuid("tmp channel guid");
        ticket.setMobileTerminalGuid(" tmp mobile terminal guid");
        ticket.setRecipient("tmp recipient");

        return ticket;
    }

    public static <T> T deserializeResponseDto(String responseDto, Class<T> clazz) throws Exception {
        ObjectMapper objectMapper = new ObjectMapper();
        ObjectNode node = objectMapper.readValue(responseDto, ObjectNode.class);
        JsonNode jsonNode = node.get("data");
        return objectMapper.readValue(objectMapper.writeValueAsString(jsonNode), clazz);
    }
}
