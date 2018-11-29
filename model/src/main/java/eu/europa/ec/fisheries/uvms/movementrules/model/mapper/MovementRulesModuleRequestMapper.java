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
import eu.europa.ec.fisheries.schema.movementrules.exchange.v1.PluginType;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketsAndRulesByMovementsRequest;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.RulesModuleMethod;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.SetMovementReportRequest;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.RawMovementType;

import javax.xml.bind.JAXBException;

public class MovementRulesModuleRequestMapper {

    private MovementRulesModuleRequestMapper() {}
    
    public static String createSetMovementReportRequest(PluginType type, RawMovementType rawMovementType, String username) {
        try {

            SetMovementReportRequest request = new SetMovementReportRequest();
            request.setMethod(RulesModuleMethod.SET_MOVEMENT_REPORT);
            request.setType(type);
            request.setUsername(username);
            request.setRequest(rawMovementType);
            return JAXBMarshaller.marshallJaxBObjectToString(request);
        } catch (JAXBException e) {
            throw new RuntimeException(e);
        }
    }

    public static String createGetTicketsAndRulesByMovementsRequest(List<String> movementsGuids) {
        try {
            GetTicketsAndRulesByMovementsRequest request = new GetTicketsAndRulesByMovementsRequest();
            request.setMethod(RulesModuleMethod.GET_TICKETS_AND_RULES_BY_MOVEMENTS);
            request.getMovementGuids().addAll(movementsGuids);

            return JAXBMarshaller.marshallJaxBObjectToString(request);
        } catch (JAXBException e) {
            throw new RuntimeException(e);
        }
    }
}