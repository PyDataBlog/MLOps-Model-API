"use strict";

/**
 * Object Properties操作工具类
 * 
 * @class Properties
 */
class Properties {
    /**
     * 在指定的对象上面创建一个不可列举的的对象
     * 
     * @static
     * @param {Object} target 目标对象
     * @param {string} key Property的key
     * @param {*} [value] Property的值
     * @returns Property的值
     */
    public static nonenumerable(target: Object, key: string, value?: any) {
        if (!target[key]) {
            Object.defineProperty(target, key, {
                enumerable: false,
                value: (typeof value === "undefined" ? {} : value),
            });
        }
        return target[key];

    }
}

export default Properties;
