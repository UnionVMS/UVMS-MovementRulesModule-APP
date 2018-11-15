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
package eu.europa.ec.fisheries.uvms.movementrules.service.boundary;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import javax.annotation.PostConstruct;
import javax.annotation.Resource;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.jms.JMSException;
import javax.jms.Queue;
import javax.jms.TextMessage;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.ext.ContextResolver;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import eu.europa.ec.fisheries.schema.movement.module.v1.MovementModuleMethod;
import eu.europa.ec.fisheries.uvms.commons.date.DateUtils;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageConstants;
import eu.europa.ec.fisheries.uvms.movement.model.exception.MovementModelException;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.bean.MovementProducerBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import eu.europa.ec.fisheries.schema.movement.module.v1.CreateMovementResponse;
import eu.europa.ec.fisheries.schema.movement.search.v1.MovementMapResponseType;
import eu.europa.ec.fisheries.schema.movement.search.v1.MovementQuery;
import eu.europa.ec.fisheries.schema.movement.search.v1.RangeCriteria;
import eu.europa.ec.fisheries.schema.movement.search.v1.RangeKeyType;
import eu.europa.ec.fisheries.schema.movement.v1.MovementBaseType;
import eu.europa.ec.fisheries.schema.movement.v1.MovementType;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.RawMovementType;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageException;
import eu.europa.ec.fisheries.uvms.movement.model.mapper.MovementModuleRequestMapper;
import eu.europa.ec.fisheries.uvms.movement.model.mapper.MovementModuleResponseMapper;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.consumer.RulesResponseConsumer;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.MovementBaseTypeMapper;

@Stateless
public class MovementServiceBean {

    private static final Logger LOG = LoggerFactory.getLogger(MovementServiceBean.class);
    
    private static final double VICINITY_RADIUS = 0.05;
    private static final long TWENTYFOUR_HOURS_IN_MILLISEC = 86400000;
    
    @Inject
    private RulesResponseConsumer consumer;

    @Inject
    private MovementProducerBean movementProducerBean;

    @Resource(mappedName = "java:/" + MessageConstants.QUEUE_MOVEMENTRULES)
    private Queue responseQueue;

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper().configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
    private Client client = ClientBuilder.newClient();

    @PostConstruct
    public void init(){
        client.register(new ContextResolver<ObjectMapper>() {
            @Override
            public ObjectMapper getContext(Class<?> type) {
                return OBJECT_MAPPER;
            }
        });
    }
    
    public MovementType sendToMovement(String connectId, RawMovementType rawMovement, String username) {
        LOG.info("Send the validated raw position to Movement..");
        MovementType createdMovement = null;
        try {
            MovementBaseType movementBaseType = MovementBaseTypeMapper.mapRawMovementFact(rawMovement);
            movementBaseType.setConnectId(connectId);
            String createMovementRequest = MovementModuleRequestMapper.mapToCreateMovementRequest(movementBaseType, username);
            String messageId = movementProducerBean.sendModuleMessage(createMovementRequest, responseQueue, MovementModuleMethod.CREATE.value(), connectId);
            TextMessage movementResponse = consumer.getMessage(messageId, TextMessage.class, 30000L);
            CreateMovementResponse createMovementResponse = MovementModuleResponseMapper.mapToCreateMovementResponseFromMovementResponse(movementResponse);
            createdMovement = createMovementResponse.getMovement();
        } catch (JMSException | MovementModelException | MessageException e) {
            LOG.error("[ERROR] Error when getting movementResponse from Movement , movementResponse from JMS Queue is null..");
        }

        return createdMovement;
    }



    public Integer numberOfReportsLast24Hours(String connectId, Date thisTime) {

        try {

            long response = client.target("http://localhost:8080/unionvms/")
                    .path("movement/rest/internal/countMovementsInDateAndTheDayBeforeForAsset/" + connectId)
                    .queryParam("after", DateUtils.dateToString(thisTime))    //yyyy-MM-dd HH:mm:ss Z
                    .request(MediaType.APPLICATION_JSON)
                    .get(long.class);

            return (int)response;
        } catch (Exception e) {
            // If something goes wrong, continue with the other validation
            LOG.warn("[ERROR] Error when fetching sum of previous movement reports:{} ]", e.getMessage());
            return null;
        }
    }
    
    public List<String> getVicinityOf(RawMovementType rawMovement) {
//      long start = System.currentTimeMillis();
      List<String> vicinityOf = new ArrayList<>();
      /*
      try {
          MovementQuery query = new MovementQuery();
          query.setExcludeFirstAndLastSegment(true);

          RangeCriteria time = new RangeCriteria();
          //GregorianCalendar from = rawMovement.getPositionTime().toGregorianCalendar();
          //from.add(Calendar.HOUR_OF_DAY, -1);
          Date fromDate = new Date(rawMovement.getPositionTime().getTime() - TWENTYFOUR_HOURS_IN_MILLISEC);
          time.setKey(RangeKeyType.DATE);
          time.setFrom(fromDate.toString());
          time.setTo(rawMovement.getPositionTime().toString());
          query.getMovementRangeSearchCriteria().add(time);

          eu.europa.ec.fisheries.schema.movement.search.v1.ListPagination pagination = new eu.europa.ec.fisheries.schema.movement.search.v1.ListPagination();
          pagination.setListSize(BigInteger.valueOf(1000L));
          pagination.setPage(BigInteger.ONE);
          query.setPagination(pagination);

          String request = MovementModuleRequestMapper.mapToGetMovementListByQueryRequest(query);
          String messageId = rulesProducer.sendDataSourceMessage(request, DataSourceQueue.MOVEMENT);
          TextMessage movementResponse = consumer.getMessage(messageId, TextMessage.class);
          List<MovementType> movements = MovementModuleResponseMapper.mapToMovementListResponse(movementResponse);
          double centerX = rawMovement.getPosition().getLongitude();
          double centerY = rawMovement.getPosition().getLatitude();
          List<String> guidList = new ArrayList<>();
          for (MovementType movement : movements) {
              if (guidList.contains(movement.getConnectId())) {
                  continue;
              }
              double x = movement.getPosition().getLongitude();
              double y = movement.getPosition().getLatitude();
              double distance = Math.pow(x - centerX, 2) + Math.pow(y - centerY, 2);
              if (distance < VICINITY_RADIUS) {
                  guidList.add(movement.getConnectId());
                  Asset asset = getAssetByConnectId(movement.getConnectId());
                  vicinityOf.add(asset.getIrcs());
              }
              //(x - center_x)^2 + (y - center_y)^2 < radius^2
          }
      } catch (AssetModelMapperException | JMSException | MessageException | ModelMapperException | MovementFaultException | MovementDuplicateException e) {
          LOG.warn("Could not fetch movements for vicinity of.");
      }

      LOG.debug("[ Get nearby vessels: {} ms ]", (System.currentTimeMillis() - start));
      */
      return vicinityOf;
  }
}
