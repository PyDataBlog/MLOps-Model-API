<?php

$output = '';
$links = '';

if ($node->nid) {
    $children_list = eds_profile_book_children($node->nid);
//echo '<pre>';
//var_dump($node);
//echo '</pre>';
//exit;
    if ($prev = book_prev($node->book)) {
        // Previous page
        drupal_add_html_head_link(array('rel' => 'prev', 'href' => url($prev['href'])));
        $links .= l(t('< ') . $prev['title'],
            $prev['href'],
            array('attributes' => array('class' => 'page-previous', 'title' => t('Go to previous page'))));
    }

//    if ($node->field_parent) {
//        // Parent page
//        drupal_add_html_head_link(array('rel' => 'up', 'href' => url('node/'. $node->field_parent['und'][0]['value'])));
//        $links .= l(t('up'),
//            'node/'. $node->field_parent['und'][0]['value'],
//            array('class' => 'page-up', 'title' => t('Go to parent page')));
//    }
    if ($node->book['plid'] && $parent = book_link_load($node->book['plid'])) {
        // Parent page
        drupal_add_html_head_link(array('rel' => 'up', 'href' => url($parent['href'])));
        $links .= l(t('up'),
            $parent['href'],
            array('attributes' => array('class' => 'page-up', 'title' => t('Go to parent page'))));
    }

    if ($next = book_next($node->book)) {
        // Next page
        drupal_add_html_head_link(array('rel' => 'next', 'href' => url($next['href'])));
        $links .= l($next['title'] . t(' >'),
            $next['href'],
            array('attributes' => array('class' => 'page-next', 'title' => t('Go to next page'))));
    }

    if ((isset($children_list) || isset($links)) && (!isset($node->no_links))) {
        // Add wrapers
        $output = '<div class="book-navigation">';
        if (isset($children_list)) {
            $output .= '<div class="book-children">'. $children_list .'</div>';
        }
        if (isset($links)) {
            $output .= '<div class="page-links clearfix">'. $links .'</div>';
        }
        $output .= '</div>';
    }
}
//echo '<pre>';
//var_dump($output);
//echo '</pre>';
//exit;
echo $output;