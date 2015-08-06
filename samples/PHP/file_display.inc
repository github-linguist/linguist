<?php

/**
 * Plugins are described by creating a $plugin array which will be used
 * by the system that includes this file.
 */
$plugin = array(
  'single' => TRUE,
  'title' => t('File display'),
  'description' => t('Displays the file with a configurable style.'),
  'required context' => new ctools_context_required(t('File'), 'entity:file'),
  'category' => t('File'),
  'defaults' => array(
    'displays' => array(),
  ),
);

/**
 * Render the node content.
 */
function file_entity_file_display_content_type_render($subtype, $conf, $panel_args, $context) {
  if (!empty($context) && empty($context->data)) {
    return;
  }
  $file = isset($context->data) ? clone($context->data) : NULL;
  $block = new stdClass();
  $block->module = 'file_entity';
  $block->delta  = $file->fid;

  if (empty($file)) {
    $block->delta   = 'placeholder';
    $block->title = t('File display');
    $block->content = t('File display goes here.');
  }
  else {
    if (!empty($conf['identifier'])) {
      $file->ctools_template_identifier = $conf['identifier'];
    }

    $block->title = $file->filename;
    $block->content = file_view_file($file, $conf['displays']);
  }

  if (!empty($conf['link']) && $file) {
    $block->title_link = entity_uri('file', $file);
  }

  return $block;
}

/**
 * Edit form for this plugin.
 */
function file_entity_file_display_content_type_edit_form($form, &$form_state) {
  $conf = $form_state['conf'];
  $form['#tree'] = TRUE;
  $form['#attached']['js'][] = drupal_get_path('module', 'file_entity') . '/file_entity.admin.js';

  // Retrieve available formatters for this file. We can load all file types
  // since we don't know which type the file is at this point.
  $formatters = file_info_formatter_types();

  // Formatter status.
  $form['displays']['status'] = array(
    '#type' => 'item',
    '#title' => t('Enabled displays'),
    '#prefix' => '<div id="file-displays-status-wrapper">',
    '#suffix' => '</div>',
  );
  $i=0;
  foreach ($formatters as $name => $formatter) {
    $form['displays']['status'][$name] = array(
      '#type' => 'checkbox',
      '#title' => check_plain($formatter['label']),
      '#default_value' => !empty($conf['displays'][$name]['status']),
      '#description' => isset($formatter['description']) ? filter_xss($formatter['description']) : NULL,
      '#parents' => array('displays', $name, 'status'),
      '#weight' => (isset($formatter['weight']) ? $formatter['weight'] : 0) + ($i / 1000),
    );
    $i++;
  }
  // Formatter order (tabledrag).
  $form['displays']['order'] = array(
    '#type' => 'item',
    '#title' => t('Display precedence order'),
    '#theme' => 'file_entity_file_display_order',
  );
  foreach ($formatters as $name => $formatter) {
    $form['displays']['order'][$name]['label'] = array(
      '#markup' => check_plain($formatter['label']),
    );
    $form['displays']['order'][$name]['weight'] = array(
      '#type' => 'weight',
      '#title' => t('Weight for @title', array('@title' => $formatter['label'])),
      '#title_display' => 'invisible',
      '#delta' => 50,
      '#default_value' => isset($conf['displays'][$name]['weight']) ? $conf['displays'][$name]['weight'] : 0,
      '#parents' => array('displays', $name, 'weight'),
    );
    $form['displays']['order'][$name]['#weight'] = $form['displays']['order'][$name]['weight']['#default_value'];
  }

  // Formatter settings.
  $form['display_settings_title'] = array(
    '#type' => 'item',
    '#title' => t('Display settings'),
  );
  $form['display_settings'] = array(
    '#type' => 'vertical_tabs',
  );
  $i=0;
  foreach ($formatters as $name => $formatter) {
    if (isset($formatter['settings callback']) && ($function = $formatter['settings callback']) && function_exists($function)) {
      $defaults = !empty($formatter['default settings']) ? $formatter['default settings'] : array();
      $settings = !empty($conf['displays'][$name]['settings']) ? $conf['displays'][$name]['settings'] : array();
      $settings += $defaults;
      $settings_form = $function($form, $form_state, $settings, $name, $file_type, $view_mode);
      if (!empty($settings_form)) {
        $form['displays']['settings'][$name] = array(
          '#type' => 'fieldset',
          '#title' => check_plain($formatter['label']),
          '#parents' => array('displays', $name, 'settings'),
          '#group' => 'display_settings',
          '#weight' => (isset($formatter['weight']) ? $formatter['weight'] : 0) + ($i / 1000),
        ) + $settings_form;
      }
    }
    $i++;
  }
  return $form;
}

function file_entity_file_display_content_type_edit_form_submit($form, &$form_state) {
  // Copy everything from our defaults.
  foreach (array_keys($form_state['plugin']['defaults']) as $key) {
    $form_state['conf'][$key] = $form_state['values'][$key];
  }
}

function file_entity_file_display_content_type_admin_title($subtype, $conf, $context) {
  return t('"@s" content', array('@s' => $context->identifier));
}
