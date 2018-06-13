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
package eu.europa.ec.fisheries.uvms.movementrules.service.dao;

import static org.junit.Assert.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import java.util.ArrayList;
import java.util.List;
import javax.persistence.EntityManager;
import javax.persistence.TypedQuery;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.runners.MockitoJUnitRunner;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;

@RunWith(MockitoJUnitRunner.class)
public class DaoBeanTest {

    @Mock
    EntityManager em;

    @InjectMocks
    private RulesDao dao;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void testCreateCustomRule() {
        CustomRule customRule = new CustomRule();

        dao.createCustomRule(customRule);
        verify(em).persist(customRule);
    }

    @Test
    public void testGetCustomRuleByGuid() {
        String guid = "sdfsadfsdsagsd";
        CustomRule entity = new CustomRule();
        entity.setGuid(guid);

        TypedQuery<CustomRule> query = mock(TypedQuery.class);
        when(em.createNamedQuery(CustomRule.FIND_CUSTOM_RULE_BY_GUID, CustomRule.class)).thenReturn(query);
        CustomRule dummyResult = new CustomRule();
        when(query.getSingleResult()).thenReturn(dummyResult);

        CustomRule result = dao.getCustomRuleByGuid(guid);
        verify(em).createNamedQuery(CustomRule.FIND_CUSTOM_RULE_BY_GUID, CustomRule.class);
        verify(query).getSingleResult();
        assertSame(dummyResult, result);
    }

    @Test
    public void testUpdateCustomRule() {
        Long id = 11L;

        CustomRule myEntity = new CustomRule();
        myEntity.setId(id);

        CustomRule result = new CustomRule();
        result.setId(id);
        when(em.merge(myEntity)).thenReturn(result);

        CustomRule resultEntity = dao.updateCustomRule(myEntity);

        verify(em).merge(myEntity);
        assertSame(id, resultEntity.getId());
    }

    @Test
    public void testDeleteCustomRule() {
        // em.remove(arg0);
    }

    @Test
    public void testGetCustomRuleList() {
        TypedQuery<CustomRule> query = mock(TypedQuery.class);
        when(em.createNamedQuery(CustomRule.LIST_CUSTOM_RULES_BY_USER, CustomRule.class)).thenReturn(query);

        List<CustomRule> dummyResult = new ArrayList<CustomRule>();
        when(query.getResultList()).thenReturn(dummyResult);

        List<CustomRule> result = dao.getCustomRulesByUser("dummyUser");

        verify(em).createNamedQuery(CustomRule.LIST_CUSTOM_RULES_BY_USER, CustomRule.class);
        verify(query).getResultList();
        assertSame(dummyResult, result);
    }
}