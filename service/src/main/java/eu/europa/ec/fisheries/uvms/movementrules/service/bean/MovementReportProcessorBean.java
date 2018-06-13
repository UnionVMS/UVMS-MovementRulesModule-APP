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
package eu.europa.ec.fisheries.uvms.movementrules.service.bean;

import java.util.Date;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.FutureTask;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.jms.JMSException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import eu.europa.ec.fisheries.schema.exchange.movement.v1.MovementRefTypeType;
import eu.europa.ec.fisheries.schema.exchange.plugin.types.v1.PluginType;
import eu.europa.ec.fisheries.schema.mobileterminal.types.v1.ComChannelAttribute;
import eu.europa.ec.fisheries.schema.mobileterminal.types.v1.ComChannelType;
import eu.europa.ec.fisheries.schema.mobileterminal.types.v1.MobileTerminalType;
import eu.europa.ec.fisheries.schema.movement.v1.MovementType;
import eu.europa.ec.fisheries.schema.movementrules.asset.v1.AssetId;
import eu.europa.ec.fisheries.schema.movementrules.mobileterminal.v1.IdList;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.MovementSourceType;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.RawMovementType;
import eu.europa.ec.fisheries.uvms.asset.model.exception.AssetModelMapperException;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageException;
import eu.europa.ec.fisheries.uvms.mobileterminal.model.exception.MobileTerminalModelMapperException;
import eu.europa.ec.fisheries.uvms.mobileterminal.model.exception.MobileTerminalUnmarshallException;
import eu.europa.ec.fisheries.uvms.movementrules.model.exception.RulesModelMapperException;
import eu.europa.ec.fisheries.uvms.movementrules.service.boundary.AssetServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.boundary.ConfigServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.boundary.ExchangeServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.boundary.MobileTerminalServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.boundary.MovementServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.MovementFact;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.RawMovementFact;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.RulesValidator;
import eu.europa.ec.fisheries.uvms.movementrules.service.dao.RulesDao;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.PreviousReport;
import eu.europa.ec.fisheries.uvms.movementrules.service.exception.RulesServiceException;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.AssetAssetIdMapper;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.MobileTerminalMapper;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.MovementFactMapper;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.RawMovementFactMapper;
import eu.europa.ec.fisheries.wsdl.asset.group.AssetGroup;
import eu.europa.ec.fisheries.wsdl.asset.types.Asset;

@Stateless
public class MovementReportProcessorBean {

    private static final Logger LOG = LoggerFactory.getLogger(MovementReportProcessorBean.class);
    
    @Inject
    private AssetServiceBean assetService;
    
    @Inject
    private MobileTerminalServiceBean mobileTerminalService;
    
    @Inject
    private ExchangeServiceBean exchangeService;
    
    @Inject
    private MovementServiceBean movementService;
    
    @Inject
    private ConfigServiceBean configService;
    
    @Inject
    private RulesValidator rulesValidator;
    
    @Inject
    private RulesDao rulesDao;
    
    public void setMovementReportReceived(final RawMovementType rawMovement, String pluginType, String username) throws RulesServiceException {
        try {
            Date auditTimestamp = new Date();
            Date auditTotalTimestamp = new Date();

            Asset asset = null;

            // Get Mobile Terminal if it exists
            MobileTerminalType mobileTerminal = mobileTerminalService.getMobileTerminalByRawMovement(rawMovement);
            auditTimestamp = auditLog("Time to fetch from Mobile Terminal Module:", auditTimestamp);

            // Get Asset
            if (mobileTerminal != null) {
                String connectId = mobileTerminal.getConnectId();
                if (connectId != null) {
                    asset = assetService.getAssetByConnectId(connectId);
                }
            } else {
                asset = assetService.getAssetByCfrIrcs(rawMovement.getAssetId());
                if (isPluginTypeWithoutMobileTerminal(rawMovement.getPluginType()) && asset != null) {
                    mobileTerminal = mobileTerminalService.findMobileTerminalByAsset(asset.getAssetId().getGuid());
                    rawMovement.setMobileTerminal(MobileTerminalMapper.mapMobileTerminal(mobileTerminal));
                }
            }
            if (rawMovement.getAssetId() == null && asset != null) {
                AssetId assetId = AssetAssetIdMapper.mapAssetToAssetId(asset);
                rawMovement.setAssetId(assetId);
            }
            auditTimestamp = auditLog("Time to fetch from Asset Module:", auditTimestamp);

            RawMovementFact rawMovementFact = RawMovementFactMapper.mapRawMovementFact(rawMovement, mobileTerminal, asset, pluginType);
            LOG.debug("rawMovementFact:{}", rawMovementFact);

            rulesValidator.evaluate(rawMovementFact);
            auditLog("Time to validate sanity:", auditTimestamp);

            if (rawMovementFact.isOk()) {
                MovementFact movementFact = collectMovementData(mobileTerminal, asset, rawMovement, username);
                LOG.info("[INFO] Validating movement from Movement Module");
                rulesValidator.evaluate(movementFact);
                auditLog("Rules total time:", auditTotalTimestamp);
                // Tell Exchange that a movement was persisted in Movement
                exchangeService.sendBackToExchange(movementFact.getMovementGuid(), rawMovement, MovementRefTypeType.MOVEMENT, username);
            } else {
                // Tell Exchange that the report caused an alarm
                exchangeService.sendBackToExchange(null, rawMovement, MovementRefTypeType.ALARM, username);
            }
        } catch (MessageException | MobileTerminalModelMapperException | MobileTerminalUnmarshallException | JMSException | AssetModelMapperException | InterruptedException | ExecutionException e) {
            throw new RulesServiceException(e.getMessage());
        }
    }

    private boolean isPluginTypeWithoutMobileTerminal(String pluginType) {
        if (pluginType == null) {
            return true;
        }
        try {
            PluginType type = PluginType.valueOf(pluginType);
            switch (type) {
                case MANUAL:
                case NAF:
                case OTHER:
                    return true;
                default:
                    return false;
            }
        } catch (IllegalArgumentException e) {
            return false;
        }
    }

    private MovementFact collectMovementData(MobileTerminalType mobileTerminal, Asset asset, final RawMovementType rawMovement, final String username) throws ExecutionException, InterruptedException, RulesServiceException {
        int threadNum = 5;
        ExecutorService executor = Executors.newFixedThreadPool(threadNum);
        Integer numberOfReportsLast24Hours;
        final String assetGuid;
        final String assetHistGuid;
        final String assetFlagState;
        if (asset != null && asset.getAssetId() != null && asset.getEventHistory() != null) {
            assetGuid = asset.getAssetId().getGuid();
            assetHistGuid = asset.getEventHistory().getEventId();
            assetFlagState = asset.getCountryCode();
        } else {
            LOG.warn("[WARN] Asset was null for {} ", rawMovement.getAssetId());
            assetGuid = null;
            assetHistGuid = null;
            assetFlagState = null;
        }

        final Date positionTime = rawMovement.getPositionTime();

        FutureTask<Long> timeDiffAndPersistMovementTask = new FutureTask<>(new Callable<Long>() {
            @Override
            public Long call() {
                return timeDiffAndPersistMovement(rawMovement.getSource(), assetGuid, assetFlagState, positionTime);
            }
        });
        executor.execute(timeDiffAndPersistMovementTask);

        FutureTask<Integer> numberOfReportsLast24HoursTask = new FutureTask<>(new Callable<Integer>() {
            @Override
            public Integer call() {
                return movementService.numberOfReportsLast24Hours(assetHistGuid, positionTime);
            }
        });
        executor.execute(numberOfReportsLast24HoursTask);

        FutureTask<MovementType> sendToMovementTask = new FutureTask<>(new Callable<MovementType>() {
            @Override
            public MovementType call() {
                return movementService.sendToMovement(assetHistGuid, rawMovement, username);
            }
        });
        executor.execute(sendToMovementTask);

        FutureTask<List<AssetGroup>> assetGroupTask = new FutureTask<>(new Callable<List<AssetGroup>>() {
            @Override
            public List<AssetGroup> call() {
                return assetService.getAssetGroup(assetGuid);
            }
        });
        executor.execute(assetGroupTask);

        FutureTask<List<String>> vicinityOfTask = new FutureTask<>(new Callable<List<String>>() {
            @Override
            public List<String> call() {
                return movementService.getVicinityOf(rawMovement);
            }
        });
        executor.execute(vicinityOfTask);

        // Get channel guid
        String channelGuid = "";
        if (mobileTerminal != null) {
            channelGuid = getChannelGuid(mobileTerminal, rawMovement);
        }

        // Get channel type
        String comChannelType = null;
        if (rawMovement.getComChannelType() != null) {
            comChannelType = rawMovement.getComChannelType().name();
        }

        // Get data from parallel tasks
        try {
            Date auditParallelTimestamp = new Date();
            Long timeDiffInSeconds = timeDiffAndPersistMovementTask.get();
            List<AssetGroup> assetGroups = assetGroupTask.get();
            numberOfReportsLast24Hours = numberOfReportsLast24HoursTask.get();
            MovementType createdMovement = sendToMovementTask.get();
            List<String> vicinityOf = vicinityOfTask.get();
            auditLog("Total time for parallel tasks:", auditParallelTimestamp);

            MovementFact movementFact = MovementFactMapper.mapMovementFact(createdMovement, mobileTerminal, asset, comChannelType, assetGroups, timeDiffInSeconds, numberOfReportsLast24Hours, channelGuid, vicinityOf);
            LOG.debug("movementFact:{}", movementFact);

            executor.shutdown();
            return movementFact;
        } catch (IllegalArgumentException | NullPointerException e) {
            executor.shutdown();
            throw new RulesServiceException("Error likely caused by a duplicate movement.", e);
        }
    }

    private Long timeDiffAndPersistMovement(MovementSourceType movementSource, String assetGuid, String assetFlagState, Date positionTime) {
        Date auditTimestamp = new Date();

        // This needs to be done before persisting last report
        Long timeDiffInSeconds = null;
        Long timeDiff = timeDiffFromLastCommunication(assetGuid, positionTime);
        timeDiffInSeconds = timeDiff != null ? timeDiff / 1000 : null;
        auditTimestamp = auditLog("Time to fetch time difference to previous report:", auditTimestamp);

        // We only persist our own last communications that were not from AIS.
        if (configService.isLocalFlagstate(assetFlagState) && !movementSource.equals(MovementSourceType.AIS)) {
            persistLastCommunication(assetGuid, positionTime);
        }
        auditLog("Time to persist the position time:", auditTimestamp);

        return timeDiffInSeconds;
    }

    private Long timeDiffFromLastCommunication(String assetGuid, Date thisTime) {
        LOG.info("[INFO] Fetching time difference to previous movement report");
        Long timeDiff = null;
        try {
            PreviousReport entity = rulesDao.getPreviousReportByAssetGuid(assetGuid);

            Date previousTime = entity.getPositionTime();
            timeDiff = thisTime.getTime() - previousTime.getTime();
        } catch (Exception e) { // there should be a DaoMappingException here but based on this exception and the below comment I am putting it in a comment instead.....
            // If something goes wrong, continue with the other validation
            LOG.error("[ERROR] Error when getting previous report by asset guid {}", e.getMessage());
            LOG.warn("[WARN] Error when fetching time difference of previous movement reports..");
        }
        return timeDiff;
    }

    private void persistLastCommunication(String assetGuid, Date positionTime) {
        PreviousReport entity = rulesDao.getPreviousReportByAssetGuid(assetGuid);
        if (entity == null) {
            entity = new PreviousReport();
        }
        entity.setPositionTime(positionTime);
        entity.setAssetGuid(assetGuid);
        entity.setUpdated(new Date());
        entity.setUpdatedBy("UVMS");
        rulesDao.updatePreviousReport(entity);
    }

    // TODO: Implement for IRIDIUM as well (if needed)
    private String getChannelGuid(MobileTerminalType mobileTerminal, RawMovementType rawMovement) {
        String dnid = "";
        String memberNumber = "";
        String channelGuid = "";

        List<IdList> ids = rawMovement.getMobileTerminal().getMobileTerminalIdList();

        for (IdList id : ids) {
            switch (id.getType()) {
                case DNID:
                    if (id.getValue() != null) {
                        dnid = id.getValue();
                    }
                    break;
                case MEMBER_NUMBER:
                    if (id.getValue() != null) {
                        memberNumber = id.getValue();
                    }
                    break;
                case SERIAL_NUMBER:
                    // IRIDIUM
                case LES:
                default:
                    LOG.error("[ERROR] Unhandled Mobile Terminal id: {} ]", id.getType());
                    break;
            }
        }

        // Get the channel guid
        boolean correctDnid = false;
        boolean correctMemberNumber = false;
        List<ComChannelType> channels = mobileTerminal.getChannels();
        for (ComChannelType channel : channels) {

            List<ComChannelAttribute> attributes = channel.getAttributes();

            for (ComChannelAttribute attribute : attributes) {
                String type = attribute.getType();
                String value = attribute.getValue();

                if ("DNID".equals(type)) {
                    correctDnid = value.equals(dnid);
                }
                if ("MEMBER_NUMBER".equals(type)) {
                    correctMemberNumber = value.equals(memberNumber);
                }
            }

            if (correctDnid && correctMemberNumber) {
                channelGuid = channel.getGuid();
            }
        }

        return channelGuid;
    }

    private Date auditLog(String msg, Date lastTimestamp) {
        Date newTimestamp = new Date();
        long duration = newTimestamp.getTime() - lastTimestamp.getTime();
        LOG.debug("[INFO] --> AUDIT - {} {} ms", msg, duration);
        return newTimestamp;
    }
}
