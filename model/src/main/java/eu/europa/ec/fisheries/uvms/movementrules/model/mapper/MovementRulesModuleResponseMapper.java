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
package eu.europa.ec.fisheries.uvms.movementrules.model.mapper;

import java.util.List;
import javax.jms.JMSException;
import javax.jms.TextMessage;
import eu.europa.ec.fisheries.schema.movementrules.common.v1.RulesFault;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.CustomRuleType;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.CountTicketsByMovementsResponse;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetCustomRuleResponse;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketsAndRulesByMovementsResponse;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketsByMovementsResponse;
import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketType;
import eu.europa.ec.fisheries.schema.movementrules.ticketrule.v1.TicketAndRuleType;
import eu.europa.ec.fisheries.uvms.movementrules.model.constant.FaultCode;
import eu.europa.ec.fisheries.uvms.movementrules.model.exception.MovementRulesFaultException;
import eu.europa.ec.fisheries.uvms.movementrules.model.exception.MovementRulesModelMapperException;
import eu.europa.ec.fisheries.uvms.movementrules.model.exception.MovementRulesModelMarshallException;

public class MovementRulesModuleResponseMapper {
    
    private MovementRulesModuleResponseMapper() {}

    private static void validateResponse(TextMessage response, String correlationId) throws MovementRulesModelMapperException, JMSException, MovementRulesFaultException {

        if (response == null) {
            throw new MovementRulesModelMapperException("Error when validating response in ResponseMapper: Reesponse is Null");
        }

        if (response.getJMSCorrelationID() == null) {
            throw new MovementRulesModelMapperException("No corelationId in response (Null) . Expected was: " + correlationId);
        }

        if (!correlationId.equalsIgnoreCase(response.getJMSCorrelationID())) {
            throw new MovementRulesModelMapperException("Wrong corelationId in response. Expected was: " + correlationId + "But actual was: " + response.getJMSCorrelationID());
        }

        try {
            RulesFault rulesFault = JAXBMarshaller.unmarshallTextMessage(response, RulesFault.class);
            throw new MovementRulesFaultException(response.getText(), rulesFault);
        } catch (MovementRulesModelMarshallException e) {
            // All is well
        }
    }

    public static String mapToGetTicketListByMovementsResponse(List<TicketType> movementList) throws MovementRulesModelMarshallException {
        GetTicketsByMovementsResponse response = new GetTicketsByMovementsResponse();
        response.getTickets().addAll(movementList);
        return JAXBMarshaller.marshallJaxBObjectToString(response);
    }

    public static String mapToCountTicketListByMovementsResponse(long count) throws MovementRulesModelMarshallException {
        CountTicketsByMovementsResponse response = new CountTicketsByMovementsResponse();
        response.setCount(count);
        return JAXBMarshaller.marshallJaxBObjectToString(response);
    }

    public static String mapToGetCustomRuleResponse(CustomRuleType rule) throws MovementRulesModelMarshallException {
        GetCustomRuleResponse response = new GetCustomRuleResponse();
        response.setCustomRule(rule);
        return JAXBMarshaller.marshallJaxBObjectToString(response);
    }

    public static GetTicketsAndRulesByMovementsResponse mapToGetTicketsAndRulesByMovementsFromResponse(TextMessage message) throws MovementRulesModelMarshallException, MovementRulesModelMapperException, JMSException, MovementRulesFaultException {
        validateResponse(message, message.getJMSCorrelationID());
        return JAXBMarshaller.unmarshallTextMessage(message, GetTicketsAndRulesByMovementsResponse.class);
    }

    public static String getTicketsAndRulesByMovementsResponse(List<TicketAndRuleType> ticketAndRuleType) throws MovementRulesModelMapperException {
        GetTicketsAndRulesByMovementsResponse response = new GetTicketsAndRulesByMovementsResponse();
        response.getTicketsAndRules().addAll(ticketAndRuleType);
        return JAXBMarshaller.marshallJaxBObjectToString(response);
    }

    public static RulesFault createFaultMessage(FaultCode code, String message) {
        RulesFault fault = new RulesFault();
        fault.setCode(code.getCode());
        fault.setMessage(message);
        return fault;
    }
    
}