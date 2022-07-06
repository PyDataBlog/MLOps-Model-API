package com.convenient.plan.service;

import net.sf.json.JSONArray;

public interface PlanIndexService {

	/**
	 * 获取所有项目的计划，并以JSONArray的格式返回
	 * @return
	 * @throws RuntimeException
	 * @throws Exception
	 */
	public JSONArray getAllProjectPlan() throws RuntimeException, Exception;
}
