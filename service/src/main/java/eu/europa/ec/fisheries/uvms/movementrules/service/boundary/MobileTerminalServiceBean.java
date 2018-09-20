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

import java.util.List;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.jms.JMSException;
import javax.jms.TextMessage;

import eu.europa.ec.fisheries.schema.mobileterminal.module.v1.MobileTerminalModuleMethod;
import eu.europa.ec.fisheries.uvms.movementrules.service.constants.ServiceConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import eu.europa.ec.fisheries.schema.mobileterminal.types.v1.ListCriteria;
import eu.europa.ec.fisheries.schema.mobileterminal.types.v1.ListPagination;
import eu.europa.ec.fisheries.schema.mobileterminal.types.v1.MobileTerminalListQuery;
import eu.europa.ec.fisheries.schema.mobileterminal.types.v1.MobileTerminalSearchCriteria;
import eu.europa.ec.fisheries.schema.mobileterminal.types.v1.MobileTerminalType;
import eu.europa.ec.fisheries.schema.mobileterminal.types.v1.SearchKey;
import eu.europa.ec.fisheries.schema.movementrules.mobileterminal.v1.IdList;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.RawMovementType;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageException;
import eu.europa.ec.fisheries.uvms.mobileterminal.model.exception.MobileTerminalModelMapperException;
import eu.europa.ec.fisheries.uvms.mobileterminal.model.exception.MobileTerminalUnmarshallException;
import eu.europa.ec.fisheries.uvms.mobileterminal.model.mapper.MobileTerminalModuleRequestMapper;
import eu.europa.ec.fisheries.uvms.mobileterminal.model.mapper.MobileTerminalModuleResponseMapper;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.constants.DataSourceQueue;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.consumer.RulesResponseConsumer;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.RulesMessageProducer;

@Stateless
public class MobileTerminalServiceBean {

    private static final Logger LOG = LoggerFactory.getLogger(MobileTerminalServiceBean.class);
    
    @Inject
    private RulesResponseConsumer consumer;

    @Inject
    private RulesMessageProducer producer;
    
    public MobileTerminalType findMobileTerminalByAsset(String assetGuid) throws MessageException, MobileTerminalModelMapperException, MobileTerminalUnmarshallException, JMSException {
        MobileTerminalListQuery query = new MobileTerminalListQuery();
        ListPagination pagination = new ListPagination();
        pagination.setListSize(2);
        pagination.setPage(1);
        MobileTerminalSearchCriteria criteria = new MobileTerminalSearchCriteria();
        ListCriteria guidCriteria = new ListCriteria();
        guidCriteria.setKey(SearchKey.CONNECT_ID);
        guidCriteria.setValue(assetGuid);
        criteria.getCriterias().add(guidCriteria);
        query.setMobileTerminalSearchCriteria(criteria);
        query.setPagination(pagination);

        String request = MobileTerminalModuleRequestMapper.createMobileTerminalListRequest(query);
        String messageId = producer.sendDataSourceMessage(request, DataSourceQueue.MOBILE_TERMINAL, ServiceConstants.METHOD, MobileTerminalModuleMethod.LIST_MOBILE_TERMINALS.value());

        TextMessage getMobileTerminalResponse = consumer.getMessage(messageId, TextMessage.class);

        List<MobileTerminalType> resultList = MobileTerminalModuleResponseMapper.mapToMobileTerminalListResponse(getMobileTerminalResponse);

        MobileTerminalType mobileTerminal = null;

        for (MobileTerminalType mobileTerminalType : resultList) {
            if (mobileTerminalType.getConnectId() != null) {
                mobileTerminal = mobileTerminalType;
                break;
            }
        }

        return mobileTerminal;
    }
    
    public MobileTerminalType getMobileTerminalByRawMovement(RawMovementType rawMovement) throws MessageException, MobileTerminalModelMapperException, MobileTerminalUnmarshallException, JMSException {
        LOG.info("[INFO] Fetch mobile terminal");
        MobileTerminalListQuery query = new MobileTerminalListQuery();

        // If no mobile terminal information exists, don't look for one
        if (rawMovement.getMobileTerminal() == null || rawMovement.getMobileTerminal().getMobileTerminalIdList() == null) {
            return null;
        }

        List<IdList> ids = rawMovement.getMobileTerminal().getMobileTerminalIdList();

        MobileTerminalSearchCriteria criteria = new MobileTerminalSearchCriteria();
        for (IdList id : ids) {
            eu.europa.ec.fisheries.schema.mobileterminal.types.v1.ListCriteria crit = new eu.europa.ec.fisheries.schema.mobileterminal.types.v1.ListCriteria();
            switch (id.getType()) {
                case DNID:
                    if (id.getValue() != null) {
                        crit.setKey(eu.europa.ec.fisheries.schema.mobileterminal.types.v1.SearchKey.DNID);
                        crit.setValue(id.getValue());
                        criteria.getCriterias().add(crit);
                    }
                    break;
                case MEMBER_NUMBER:
                    if (id.getValue() != null) {
                        crit.setKey(eu.europa.ec.fisheries.schema.mobileterminal.types.v1.SearchKey.MEMBER_NUMBER);
                        crit.setValue(id.getValue());
                        criteria.getCriterias().add(crit);
                    }
                    break;
                case SERIAL_NUMBER:
                    if (id.getValue() != null) {
                        crit.setKey(eu.europa.ec.fisheries.schema.mobileterminal.types.v1.SearchKey.SERIAL_NUMBER);
                        crit.setValue(id.getValue());
                        criteria.getCriterias().add(crit);
                    }
                    break;
                case LES:
                default:
                    LOG.error("[ERROR] Unhandled Mobile Terminal id: {} ]", id.getType());
                    break;
            }
        }

        // If no valid criterias, don't look for a mobile terminal
        if (criteria.getCriterias().isEmpty()) {
            return null;
        }

        // If we know the transponder type from the source, use it in the search criteria
        eu.europa.ec.fisheries.schema.mobileterminal.types.v1.ListCriteria transponderTypeCrit = new eu.europa.ec.fisheries.schema.mobileterminal.types.v1.ListCriteria();
        transponderTypeCrit.setKey(eu.europa.ec.fisheries.schema.mobileterminal.types.v1.SearchKey.TRANSPONDER_TYPE);
        transponderTypeCrit.setValue(rawMovement.getSource().name());
        criteria.getCriterias().add(transponderTypeCrit);

        query.setMobileTerminalSearchCriteria(criteria);
        eu.europa.ec.fisheries.schema.mobileterminal.types.v1.ListPagination pagination = new eu.europa.ec.fisheries.schema.mobileterminal.types.v1.ListPagination();
        // To leave room to find erroneous results - it must be only one in the list
        pagination.setListSize(2);
        pagination.setPage(1);
        query.setPagination(pagination);

        String getMobileTerminalListRequest = MobileTerminalModuleRequestMapper.createMobileTerminalListRequest(query);
        String getMobileTerminalMessageId = producer.sendDataSourceMessage(getMobileTerminalListRequest, DataSourceQueue.MOBILE_TERMINAL, ServiceConstants.METHOD, MobileTerminalModuleMethod.LIST_MOBILE_TERMINALS.value());
        TextMessage getMobileTerminalResponse = consumer.getMessage(getMobileTerminalMessageId, TextMessage.class);

        List<MobileTerminalType> resultList = MobileTerminalModuleResponseMapper.mapToMobileTerminalListResponse(getMobileTerminalResponse);

        return resultList.size() != 1 ? null : resultList.get(0);
    }
  
}
