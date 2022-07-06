<?php
/**
 * Created by PhpStorm.
 * User: TT
 * Date: 2016/2/4
 * Time: 21:08
 */defined('BASEPATH') OR exit('No direct script access allowed');
?><!DOCTYPE html>
<div style="width:100%;background-color: #ffffff">
    <table>
        <tr>
            <td>表演者名称</td>
            <td>语言类型</td>
            <td>表演者照片</td>
            <td>顺序</td>
            <td>照片缩略图</td>
            <td>编辑</td>
        </tr>
        {shows}
        <tr>
            <td id="{keyword_id}">{keyword}</td>
            <td id="{keyword_id}_language">{language}</td>
            <td id="{keyword_id}_logo">
                {selections}
            </td>
            <td>
                <input type="number" value="{sequence}" name="sequence" id="{keyword_id}_sequence">
            </td>
            <td id="{source_id}_img">
                <img src="{imgurl}" style="width:96px;height: 54px;">
            </td>
            <td id="{keyword_id}_edit">
                <a href="javascript:;" onclick="$saveshower('{keyword_id}')">保存</a>
            </td>
        </tr>
        {/shows}
    </table>
</div>