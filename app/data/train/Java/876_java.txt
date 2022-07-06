package com.tianyu.mesimp.survey.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.stereotype.Service;

import com.tianyu.mesimp.survey.bean.CompanyNum;

@Service
public class SunrveyService {

	private Map<String, CompanyNum> testNumMap = new HashMap<String, CompanyNum>();
	
	public SunrveyService(){
		testNumMap.put("110000", new CompanyNum("110000", "沈阳市", 7834,1));
		testNumMap.put("116000",new CompanyNum("116000", "大连市", 42305,2));
		testNumMap.put("114000",new CompanyNum("114000", "鞍山市", 2040,3));
		testNumMap.put("113000", new CompanyNum("113000", "抚顺市", 2589,4));
		testNumMap.put("117000",new CompanyNum("117000", "本溪市", 706,5));
		testNumMap.put("118000",new CompanyNum("118000", "丹东市", 21456,6));
		testNumMap.put("121000",new CompanyNum("121000", "锦州市", 9831,7));
		testNumMap.put("115000",new CompanyNum("115000", "营口市", 25230,8));
		testNumMap.put("123000",new CompanyNum("123000", "阜新市", 132,9));
		testNumMap.put("111000",new CompanyNum("111000", "辽阳市", 98,10));
		testNumMap.put("124000",new CompanyNum("124000", "盘锦市", 10234,11));
		testNumMap.put("112000",new CompanyNum("112000", "铁岭市", 348,12));
		testNumMap.put("122000",new CompanyNum("122000", "朝阳市", 143,13));
		testNumMap.put("125000",new CompanyNum("125000", "葫芦岛市", 12387,14));
	}
	
	public List<CompanyNum> getCompanyNumData(String codes) throws Exception{
		List<CompanyNum> datalist = new ArrayList<CompanyNum>();
		if("liaoning".equals(codes)){
			datalist.addAll(testNumMap.values());
		}else{
			datalist.add(new CompanyNum("116000", "中山区", 9831,1));
			datalist.add(new CompanyNum("116000", "西岗区", 4835,2));
			datalist.add(new CompanyNum("116000", "沙河口区", 1234,3));
			datalist.add(new CompanyNum("116000", "甘井子区", 2123,4));
			datalist.add(new CompanyNum("116000", "旅顺口区", 7912,5));
			datalist.add(new CompanyNum("116000", "金州新区", 3214,6));
			datalist.add(new CompanyNum("116400", "庄河市", 2763,7));
			datalist.add(new CompanyNum("116300", "瓦房店市", 2031,8));
			datalist.add(new CompanyNum("116200", "普兰店区", 2341,9));
			datalist.add(new CompanyNum("116500", "长海县", 6021,10));
		}
		return datalist;
	}

	
}
