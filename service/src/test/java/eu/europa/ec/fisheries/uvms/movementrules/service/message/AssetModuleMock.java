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
package eu.europa.ec.fisheries.uvms.movementrules.service.message;

import java.util.ArrayList;
import java.util.UUID;
import javax.ejb.ActivationConfigProperty;
import javax.ejb.MessageDriven;
import javax.inject.Inject;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.TextMessage;
import eu.europa.ec.fisheries.uvms.asset.model.mapper.AssetModuleResponseMapper;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.RulesMessageProducer;
import eu.europa.ec.fisheries.uvms.movementrules.model.mapper.JAXBMarshaller;
import eu.europa.ec.fisheries.wsdl.asset.group.AssetGroup;
import eu.europa.ec.fisheries.wsdl.asset.module.AssetModuleMethod;
import eu.europa.ec.fisheries.wsdl.asset.module.AssetModuleRequest;
import eu.europa.ec.fisheries.wsdl.asset.module.GetAssetModuleRequest;
import eu.europa.ec.fisheries.wsdl.asset.types.Asset;
import eu.europa.ec.fisheries.wsdl.asset.types.AssetHistoryId;
import eu.europa.ec.fisheries.wsdl.asset.types.AssetId;

@MessageDriven(mappedName = "jms/queue/UVMSAssetEvent", activationConfig = {
        @ActivationConfigProperty(propertyName = "messagingType", propertyValue = "javax.jms.MessageListener"), 
        @ActivationConfigProperty(propertyName = "destinationType", propertyValue = "javax.jms.Queue"), 
        @ActivationConfigProperty(propertyName = "destination", propertyValue = "UVMSAssetEvent")})
public class AssetModuleMock implements MessageListener {
    
    @Inject
    RulesMessageProducer messageProducer;

    @Override
    public void onMessage(Message message) {
        try {
            TextMessage textMessage = (TextMessage) message;
            AssetModuleRequest request = JAXBMarshaller.unmarshallTextMessage(textMessage, AssetModuleRequest.class);
            AssetModuleMethod method = request.getMethod();

            switch (method) {
                case GET_ASSET:
                    GetAssetModuleRequest getRequest = JAXBMarshaller.unmarshallTextMessage(textMessage, GetAssetModuleRequest.class);
                    String ircs = getRequest.getId().getValue();
                    
                    Asset asset = new Asset();
                    asset.setIrcs(ircs);
                    AssetId assetId = new AssetId();
                    assetId.setGuid(UUID.randomUUID().toString());
                    asset.setAssetId(assetId);
                    AssetHistoryId assetHistoryId = new AssetHistoryId();
                    assetHistoryId.setEventId(UUID.randomUUID().toString());
                    asset.setEventHistory(assetHistoryId);
                    asset.setName("Test Asset");
                    asset.setCountryCode("SWE");
                    String response = AssetModuleResponseMapper.mapAssetModuleResponse(asset);
                    messageProducer.sendModuleResponseMessage(textMessage, response);
                    break;
                case ASSET_GROUP_LIST_BY_ASSET_GUID:
                    String groupResponse = AssetModuleResponseMapper.mapToAssetGroupListResponse(new ArrayList<AssetGroup>());
                    messageProducer.sendModuleResponseMessage(textMessage, groupResponse);
                    break;
                default:
                    break;
            }
        } catch (Exception e) {

        }
    }
}