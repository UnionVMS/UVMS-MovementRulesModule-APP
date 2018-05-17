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
package eu.europa.ec.fisheries.uvms.rules.message;

import java.util.ArrayList;
import java.util.UUID;
import javax.ejb.ActivationConfigProperty;
import javax.ejb.MessageDriven;
import javax.inject.Inject;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.TextMessage;
import eu.europa.ec.fisheries.schema.movement.module.v1.CreateMovementRequest;
import eu.europa.ec.fisheries.schema.movement.module.v1.MovementBaseRequest;
import eu.europa.ec.fisheries.schema.movement.search.v1.MovementMapResponseType;
import eu.europa.ec.fisheries.schema.movement.v1.MovementBaseType;
import eu.europa.ec.fisheries.schema.movement.v1.MovementType;
import eu.europa.ec.fisheries.uvms.movement.model.mapper.JAXBMarshaller;
import eu.europa.ec.fisheries.uvms.movement.model.mapper.MovementModuleResponseMapper;
import eu.europa.ec.fisheries.uvms.rules.message.producer.RulesMessageProducer;

@MessageDriven(mappedName = "jms/queue/UVMSAssetEvent", activationConfig = {
        @ActivationConfigProperty(propertyName = "messagingType", propertyValue = "javax.jms.MessageListener"), 
        @ActivationConfigProperty(propertyName = "destinationType", propertyValue = "javax.jms.Queue"), 
        @ActivationConfigProperty(propertyName = "destination", propertyValue = "UVMSMovementEvent")})
public class MovementModuleMock implements MessageListener {
    
    @Inject
    RulesMessageProducer messageProducer;

    @Override
    public void onMessage(Message message) {
        try {
            TextMessage textMessage = (TextMessage) message;
            MovementBaseRequest request = JAXBMarshaller.unmarshallTextMessage(textMessage, MovementBaseRequest.class);
            
            switch (request.getMethod()) {
                case CREATE:
                    CreateMovementRequest createMovementRequest = JAXBMarshaller.unmarshallTextMessage(textMessage, CreateMovementRequest.class);
                    MovementBaseType movement = createMovementRequest.getMovement();
                    MovementType movementType = toMovementType(movement);
                    
                    String response = MovementModuleResponseMapper.mapToCreateMovementResponse(movementType);
                    messageProducer.sendModuleResponseMessage((TextMessage) message, response);
                    break;
                case MOVEMENT_MAP:
                    String responseString = MovementModuleResponseMapper.mapToMovementMapResponse(new ArrayList<MovementMapResponseType>());
                    messageProducer.sendModuleResponseMessage((TextMessage) message, responseString);
                    break;
                default:
                    break;
            }
            
        } catch (Exception e) {
        }
    }
    
    private static MovementType toMovementType(MovementBaseType movement) {
        MovementType movementType = new MovementType();
        movementType.setGuid(UUID.randomUUID().toString());
        movementType.setConnectId(movementType.getConnectId());
        movementType.setAssetId(movement.getAssetId());
        movementType.setStatus(movement.getStatus());
        movementType.setPosition(movement.getPosition());
        movementType.setPositionTime(movement.getPositionTime());
        movementType.setReportedSpeed(movement.getReportedSpeed());
        movementType.setReportedCourse(movement.getReportedCourse());
        movementType.setMovementType(movement.getMovementType());
        movementType.setSource(movement.getSource());
        movementType.setActivity(movement.getActivity());
        movementType.setTripNumber(movement.getTripNumber());
        return movementType;
    }
}