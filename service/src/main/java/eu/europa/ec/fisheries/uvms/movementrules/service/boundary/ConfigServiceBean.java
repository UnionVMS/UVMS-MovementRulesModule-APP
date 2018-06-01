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

import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.jms.TextMessage;
import eu.europa.ec.fisheries.schema.config.module.v1.SettingsListResponse;
import eu.europa.ec.fisheries.schema.config.types.v1.SettingType;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageException;
import eu.europa.ec.fisheries.uvms.config.model.mapper.ModuleRequestMapper;
import eu.europa.ec.fisheries.uvms.movementrules.message.constants.DataSourceQueue;
import eu.europa.ec.fisheries.uvms.movementrules.message.consumer.RulesResponseConsumer;
import eu.europa.ec.fisheries.uvms.movementrules.message.producer.RulesMessageProducer;

@Stateless
public class ConfigServiceBean {

    @Inject
    private RulesResponseConsumer consumer;

    @Inject
    private RulesMessageProducer producer;
    
    public boolean isLocalFlagstate(String assetFlagState) {
        if (assetFlagState == null) {
            return false;
        }
        TextMessage response;
        try {
            String settingsRequest = ModuleRequestMapper.toListSettingsRequest("asset");
            String messageId = producer.sendDataSourceMessage(settingsRequest, DataSourceQueue.CONFIG);
            response = consumer.getMessage(messageId, TextMessage.class);
            SettingsListResponse settings = eu.europa.ec.fisheries.uvms.config.model.mapper.JAXBMarshaller.unmarshallTextMessage(response, SettingsListResponse.class);
            for (SettingType setting : settings.getSettings()) {
                if (setting.getKey().equals("asset.default.flagstate")) {
                    return assetFlagState.equalsIgnoreCase(setting.getValue());
                }
            }
        } catch (eu.europa.ec.fisheries.uvms.config.model.exception.ModelMapperException | MessageException e) {
            return false;
        }
        return false;
    }
}
