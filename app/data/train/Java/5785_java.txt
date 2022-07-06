package com.notronix.lw.impl.method.orders;

import com.google.gson.Gson;
import com.notronix.lw.api.model.UserOrderView;
import com.notronix.lw.impl.method.AbstractLinnworksAPIMethod;

import java.util.Arrays;
import java.util.List;

public class GetOrderViewsMethod extends AbstractLinnworksAPIMethod<List<UserOrderView>>
{
    @Override
    public String getURI() {
        return "Orders/GetOrderViews";
    }

    @Override
    public List<UserOrderView> getResponse(Gson gson, String jsonPayload) {
        return Arrays.asList(gson.fromJson(jsonPayload, UserOrderView[].class));
    }
}
