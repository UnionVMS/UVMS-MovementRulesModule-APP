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
import javax.jms.TextMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import eu.europa.ec.fisheries.schema.rules.asset.v1.AssetId;
import eu.europa.ec.fisheries.schema.rules.asset.v1.AssetIdList;
import eu.europa.ec.fisheries.uvms.asset.model.exception.AssetModelMapperException;
import eu.europa.ec.fisheries.uvms.asset.model.exception.AssetModelValidationException;
import eu.europa.ec.fisheries.uvms.asset.model.mapper.AssetModuleRequestMapper;
import eu.europa.ec.fisheries.uvms.asset.model.mapper.AssetModuleResponseMapper;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageException;
import eu.europa.ec.fisheries.uvms.rules.message.constants.DataSourceQueue;
import eu.europa.ec.fisheries.uvms.rules.message.consumer.RulesResponseConsumer;
import eu.europa.ec.fisheries.uvms.rules.message.producer.RulesMessageProducer;
import eu.europa.ec.fisheries.wsdl.asset.group.AssetGroup;
import eu.europa.ec.fisheries.wsdl.asset.types.Asset;
import eu.europa.ec.fisheries.wsdl.asset.types.AssetIdType;
import eu.europa.ec.fisheries.wsdl.asset.types.AssetListCriteria;
import eu.europa.ec.fisheries.wsdl.asset.types.AssetListCriteriaPair;
import eu.europa.ec.fisheries.wsdl.asset.types.AssetListPagination;
import eu.europa.ec.fisheries.wsdl.asset.types.AssetListQuery;
import eu.europa.ec.fisheries.wsdl.asset.types.ConfigSearchField;

@Stateless
public class AssetServiceBean {

    private static final Logger LOG = LoggerFactory.getLogger(AssetServiceBean.class);
    
    @Inject
    private RulesResponseConsumer consumer;

    @Inject
    private RulesMessageProducer producer;
    
    public Asset getAssetByConnectId(String connectId) throws AssetModelMapperException, MessageException {
        LOG.info("[INFO] Fetch asset by connectId '{}'", connectId);

        AssetListQuery query = new AssetListQuery();
        AssetListCriteria criteria = new AssetListCriteria();
        AssetListCriteriaPair criteriaPair = new AssetListCriteriaPair();
        criteriaPair.setKey(ConfigSearchField.GUID);
        criteriaPair.setValue(connectId);
        criteria.getCriterias().add(criteriaPair);
        criteria.setIsDynamic(true);

        query.setAssetSearchCriteria(criteria);

        AssetListPagination pagination = new AssetListPagination();
        // To leave room to find erroneous results - it must be only one in the list
        pagination.setListSize(2);
        pagination.setPage(1);
        query.setPagination(pagination);

        String getAssetRequest = AssetModuleRequestMapper.createAssetListModuleRequest(query);
        String getAssetMessageId = producer.sendDataSourceMessage(getAssetRequest, DataSourceQueue.ASSET);
        TextMessage getAssetResponse = consumer.getMessage(getAssetMessageId, TextMessage.class);

        List<Asset> resultList = AssetModuleResponseMapper.mapToAssetListFromResponse(getAssetResponse, getAssetMessageId);

        return resultList.size() != 1 ? null : resultList.get(0);
    }
    
    public Asset getAssetByCfrIrcs(AssetId assetId) {
        LOG.info("[INFO] Fetch asset by assetId");

        Asset asset = null;
        try {
            // If no asset information exists, don't look for one
            if (assetId == null || assetId.getAssetIdList() == null) {
                LOG.warn("No asset information exists!");
                return null;
            }

            List<AssetIdList> ids = assetId.getAssetIdList();

            String cfr = null;
            String ircs = null;
            String mmsi = null;

            // Get possible search parameters
            for (AssetIdList id : ids) {
                if (eu.europa.ec.fisheries.schema.rules.asset.v1.AssetIdType.CFR.equals(id.getIdType())) {
                    cfr = id.getValue();
                }
                if (eu.europa.ec.fisheries.schema.rules.asset.v1.AssetIdType.IRCS.equals(id.getIdType())) {
                    ircs = id.getValue();
                }
                if (eu.europa.ec.fisheries.schema.rules.asset.v1.AssetIdType.MMSI.equals(id.getIdType())) {
                    mmsi = id.getValue();
                }

            }

            if (ircs != null && cfr != null && mmsi != null) {
                try {
                    asset = getAsset(AssetIdType.CFR, cfr);
                    // If the asset matches on ircs as well we have a winner
                    if (asset != null && asset.getIrcs().equals(ircs)) {
                        return asset;
                    }
                    // If asset is null, try fetching by IRCS (cfr will fail for SE national db)
                    if (asset == null) {
                        asset = getAsset(AssetIdType.IRCS, ircs);
                        // If asset is still null, try mmsi (this should be the case for movement coming from AIS)
                        if (asset == null) {
                            return getAsset(AssetIdType.MMSI, mmsi);
                        }
                    }
                } catch (AssetModelValidationException e) {
                    return getAsset(AssetIdType.IRCS, ircs);
                }
            } else if (cfr != null) {
                return getAsset(AssetIdType.CFR, cfr);
            } else if (ircs != null) {
                return getAsset(AssetIdType.IRCS, ircs);
            } else if (mmsi != null) {
                return getAsset(AssetIdType.MMSI, mmsi);
            }

        } catch (Exception e) {
            // Log and continue validation
            LOG.warn("Could not find asset!");
        }
        return null;
    }
    
    private Asset getAsset(AssetIdType type, String value) throws AssetModelMapperException, MessageException {
        String getAssetListRequest = AssetModuleRequestMapper.createGetAssetModuleRequest(value, type);
        String getAssetMessageId = producer.sendDataSourceMessage(getAssetListRequest, DataSourceQueue.ASSET);
        TextMessage getAssetResponse = consumer.getMessage(getAssetMessageId, TextMessage.class);

        return AssetModuleResponseMapper.mapToAssetFromResponse(getAssetResponse, getAssetMessageId);
    }
    
    public List<AssetGroup> getAssetGroup(String assetGuid) {
        LOG.info("Fetch asset groups from Asset");

        List<AssetGroup> assetGroups = null;
        try {
            String getAssetRequest = AssetModuleRequestMapper.createAssetGroupListByAssetGuidRequest(assetGuid);
            String getAssetMessageId = producer.sendDataSourceMessage(getAssetRequest, DataSourceQueue.ASSET);
            TextMessage getAssetResponse = consumer.getMessage(getAssetMessageId, TextMessage.class);

            assetGroups = AssetModuleResponseMapper.mapToAssetGroupListFromResponse(getAssetResponse, getAssetMessageId);
        } catch (AssetModelMapperException | MessageException e) {
            LOG.warn("[ Failed while fetching asset groups ]", e.getMessage());
        }
        return assetGroups;
    }
}
