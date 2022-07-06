/*
 * Copyright 2016 Netbrasoft
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */

package br.com.netbrasoft.gnuob.api.category;

import static br.com.netbrasoft.gnuob.api.category.CategoryWebServiceWrapperHelper.wrapToCountCategory;
import static br.com.netbrasoft.gnuob.api.category.CategoryWebServiceWrapperHelper.wrapToFindCategory;
import static br.com.netbrasoft.gnuob.api.category.CategoryWebServiceWrapperHelper.wrapToFindCategoryById;
import static br.com.netbrasoft.gnuob.api.category.CategoryWebServiceWrapperHelper.wrapToMergeCategory;
import static br.com.netbrasoft.gnuob.api.category.CategoryWebServiceWrapperHelper.wrapToPersistCategory;
import static br.com.netbrasoft.gnuob.api.category.CategoryWebServiceWrapperHelper.wrapToRefreshCategory;
import static br.com.netbrasoft.gnuob.api.category.CategoryWebServiceWrapperHelper.wrapToRemoveCategory;
import static br.com.netbrasoft.gnuob.api.generic.NetbrasoftApiConstants.CAN_NOT_INITIALIZE_THE_DEFAULT_WSDL_FROM_0;
import static br.com.netbrasoft.gnuob.api.generic.NetbrasoftApiConstants.CATEGORY_WEB_SERVICE_REPOSITORY_NAME;
import static br.com.netbrasoft.gnuob.api.generic.NetbrasoftApiConstants.GNUOB_SOAP_CATEGORY_WEBSERVICE_WSDL;
import static br.com.netbrasoft.gnuob.api.generic.NetbrasoftApiConstants.HTTP_LOCALHOST_8080_GNUOB_SOAP_CATEGORY_WEB_SERVICE_IMPL_WSDL;
import static br.com.netbrasoft.gnuob.api.generic.NetbrasoftApiConstants.UNCHECKED_VALUE;
import static java.lang.System.getProperty;
import static org.slf4j.LoggerFactory.getLogger;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;

import org.javasimon.aop.Monitored;
import org.slf4j.Logger;
import org.springframework.stereotype.Repository;

import br.com.netbrasoft.gnuob.api.Category;
import br.com.netbrasoft.gnuob.api.CategoryWebServiceImpl;
import br.com.netbrasoft.gnuob.api.CategoryWebServiceImplService;
import br.com.netbrasoft.gnuob.api.MetaData;
import br.com.netbrasoft.gnuob.api.OrderBy;
import br.com.netbrasoft.gnuob.api.Paging;
import br.com.netbrasoft.gnuob.api.generic.IGenericTypeWebServiceRepository;

@Monitored
@Repository(CATEGORY_WEB_SERVICE_REPOSITORY_NAME)
public class CategoryWebServiceRepository<C extends Category> implements IGenericTypeWebServiceRepository<C> {

  private static final Logger LOGGER = getLogger(CategoryWebServiceRepository.class);
  private static final URL WSDL_LOCATION;

  static {
    URL url = null;
    try {
      url = new URL(getProperty(GNUOB_SOAP_CATEGORY_WEBSERVICE_WSDL,
          HTTP_LOCALHOST_8080_GNUOB_SOAP_CATEGORY_WEB_SERVICE_IMPL_WSDL));
    } catch (final MalformedURLException e) {
      LOGGER.info(CAN_NOT_INITIALIZE_THE_DEFAULT_WSDL_FROM_0, getProperty(GNUOB_SOAP_CATEGORY_WEBSERVICE_WSDL,
          HTTP_LOCALHOST_8080_GNUOB_SOAP_CATEGORY_WEB_SERVICE_IMPL_WSDL));
    }
    WSDL_LOCATION = url;
  }

  private transient CategoryWebServiceImpl categoryWebServiceImpl = null;

  private CategoryWebServiceImpl getCategoryWebServiceImpl() {
    if (categoryWebServiceImpl == null) {
      categoryWebServiceImpl = new CategoryWebServiceImplService(WSDL_LOCATION).getCategoryWebServiceImplPort();
    }
    return categoryWebServiceImpl;
  }

  @Override
  public long count(final MetaData credentials, final C categoryExample) {
    return getCategoryWebServiceImpl().countCategory(wrapToCountCategory(categoryExample), credentials).getReturn();
  }

  @Override
  @SuppressWarnings(UNCHECKED_VALUE)
  public List<C> find(final MetaData credentials, final C categoryExample, final Paging paging,
      final OrderBy orderingProperty) {
    return (List<C>) getCategoryWebServiceImpl()
        .findCategory(wrapToFindCategory(categoryExample, paging, orderingProperty), credentials).getReturn();
  }

  @Override
  @SuppressWarnings(UNCHECKED_VALUE)
  public C find(final MetaData credentials, final C categoryExample) {
    return (C) getCategoryWebServiceImpl().findCategoryById(wrapToFindCategoryById(categoryExample), credentials)
        .getReturn();
  }

  @Override
  @SuppressWarnings(UNCHECKED_VALUE)
  public C persist(final MetaData credentials, final C category) {
    return (C) getCategoryWebServiceImpl().persistCategory(wrapToPersistCategory(category), credentials).getReturn();
  }

  @Override
  @SuppressWarnings(UNCHECKED_VALUE)
  public C merge(final MetaData credentials, final C category) {
    return (C) getCategoryWebServiceImpl().mergeCategory(wrapToMergeCategory(category), credentials).getReturn();
  }

  @Override
  @SuppressWarnings(UNCHECKED_VALUE)
  public C refresh(final MetaData credentials, final C category) {
    return (C) getCategoryWebServiceImpl().refreshCategory(wrapToRefreshCategory(category), credentials).getReturn();
  }

  @Override
  public void remove(final MetaData credentials, final C category) {
    getCategoryWebServiceImpl().removeCategory(wrapToRemoveCategory(category), credentials);
  }
}
