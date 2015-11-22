<?php

/**
 * @file
 * Additional filter for PHP input.
 */

/**
 * Implements hook_help().
 */
function php_help($path, $arg) {
  switch ($path) {
    case 'admin/help#php':
      $output = '';
      $output .= '<h3>' . t('About') . '</h3>';
      $output .= '<p>' . t('The PHP filter module adds a PHP filter to your site, for use with <a href="@filter">text formats</a>. This filter adds the ability to execute PHP code in any text field that uses a text format (such as the body of a content item or the text of a comment). <a href="@php-net">PHP</a> is a general-purpose scripting language widely-used for web development, and is the language with which Drupal has been developed. For more information, see the online handbook entry for the <a href="@php">PHP filter module</a>.', array('@filter' => url('admin/help/filter'), '@php-net' => 'http://www.php.net', '@php' => 'http://drupal.org/handbook/modules/php/')) . '</p>';
      $output .= '<h3>' . t('Uses') . '</h3>';
      $output .= '<dl>';
      $output .= '<dt>' . t('Enabling execution of PHP in text fields') . '</dt>';
      $output .= '<dd>' . t('The PHP filter module allows users with the proper permissions to include custom PHP code that will get executed when pages of your site are processed. While this is a powerful and flexible feature if used by a trusted user with PHP experience, it is a significant and dangerous security risk in the hands of a malicious or inexperienced user. Even a trusted user may accidentally compromise the site by entering malformed or incorrect PHP code. Only the most trusted users should be granted permission to use the PHP filter, and all PHP code added through the PHP filter should be carefully examined before use. <a href="@php-snippets">Example PHP snippets</a> can be found on Drupal.org.', array('@php-snippets' => url('http://drupal.org/handbook/customization/php-snippets'))) . '</dd>';
      $output .= '</dl>';
      return $output;
  }
}

/**
 * Implements hook_permission().
 */
function php_permission() {
  return array(
    'use PHP for settings' => array(
      'title' => t('Use PHP for settings'),
      'restrict access' => TRUE,
    ),
  );
}

/**
 * Evaluate a string of PHP code.
 *
 * This is a wrapper around PHP's eval(). It uses output buffering to capture both
 * returned and printed text. Unlike eval(), we require code to be surrounded by
 * <?php ?> tags; in other words, we evaluate the code as if it were a stand-alone
 * PHP file.
 *
 * Using this wrapper also ensures that the PHP code which is evaluated can not
 * overwrite any variables in the calling code, unlike a regular eval() call.
 *
 * @param $code
 *   The code to evaluate.
 * @return
 *   A string containing the printed output of the code, followed by the returned
 *   output of the code.
 *
 * @ingroup php_wrappers
 */
function php_eval($code) {
  global $theme_path, $theme_info, $conf;

  // Store current theme path.
  $old_theme_path = $theme_path;

  // Restore theme_path to the theme, as long as php_eval() executes,
  // so code evaluated will not see the caller module as the current theme.
  // If theme info is not initialized get the path from theme_default.
  if (!isset($theme_info)) {
    $theme_path = drupal_get_path('theme', $conf['theme_default']);
  }
  else {
    $theme_path = dirname($theme_info->filename);
  }

  ob_start();
  print eval('?>' . $code);
  $output = ob_get_contents();
  ob_end_clean();

  // Recover original theme path.
  $theme_path = $old_theme_path;

  return $output;
}

/**
 * Tips callback for php filter.
 */
function _php_filter_tips($filter, $format, $long = FALSE) {
  global $base_url;
  if ($long) {
    $output = '<h4>' . t('Using custom PHP code') . '</h4>';
    $output .= '<p>' . t('Custom PHP code may be embedded in some types of site content, including posts and blocks. While embedding PHP code inside a post or block is a powerful and flexible feature when used by a trusted user with PHP experience, it is a significant and dangerous security risk when used improperly. Even a small mistake when posting PHP code may accidentally compromise your site.') . '</p>';
    $output .= '<p>' . t('If you are unfamiliar with PHP, SQL, or Drupal, avoid using custom PHP code within posts. Experimenting with PHP may corrupt your database, render your site inoperable, or significantly compromise security.') . '</p>';
    $output .= '<p>' . t('Notes:') . '</p>';
    $output .= '<ul><li>' . t('Remember to double-check each line for syntax and logic errors <strong>before</strong> saving.') . '</li>';
    $output .= '<li>' . t('Statements must be correctly terminated with semicolons.') . '</li>';
    $output .= '<li>' . t('Global variables used within your PHP code retain their values after your script executes.') . '</li>';
    $output .= '<li>' . t('<code>register_globals</code> is <strong>turned off</strong>. If you need to use forms, understand and use the functions in <a href="@formapi">the Drupal Form API</a>.', array('@formapi' => url('http://api.drupal.org/api/group/form_api/7'))) . '</li>';
    $output .= '<li>' . t('Use a <code>print</code> or <code>return</code> statement in your code to output content.') . '</li>';
    $output .= '<li>' . t('Develop and test your PHP code using a separate test script and sample database before deploying on a production site.') . '</li>';
    $output .= '<li>' . t('Consider including your custom PHP code within a site-specific module or <code>template.php</code> file rather than embedding it directly into a post or block.') . '</li>';
    $output .= '<li>' . t('Be aware that the ability to embed PHP code within content is provided by the PHP Filter module. If this module is disabled or deleted, then blocks and posts with embedded PHP may display, rather than execute, the PHP code.') . '</li></ul>';
    $output .= '<p>' . t('A basic example: <em>Creating a "Welcome" block that greets visitors with a simple message.</em>') . '</p>';
    $output .= '<ul><li>' . t('<p>Add a custom block to your site, named "Welcome" . With its text format set to "PHP code" (or another format supporting PHP input), add the following in the Block body:</p>
<pre>
print t(\'Welcome visitor! Thank you for visiting.\');
</pre>') . '</li>';
    $output .= '<li>' . t('<p>To display the name of a registered user, use this instead:</p>
<pre>
global $user;
if ($user->uid) {
  print t(\'Welcome @name! Thank you for visiting.\', array(\'@name\' => format_username($user)));
}
else {
  print t(\'Welcome visitor! Thank you for visiting.\');
}
</pre>') . '</li></ul>';
    $output .= '<p>' . t('<a href="@drupal">Drupal.org</a> offers <a href="@php-snippets">some example PHP snippets</a>, or you can create your own with some PHP experience and knowledge of the Drupal system.', array('@drupal' => url('http://drupal.org'), '@php-snippets' => url('http://drupal.org/handbook/customization/php-snippets'))) . '</p>';
    return $output;
  }
  else {
    return t('You may post PHP code. You should include &lt;?php ?&gt; tags.');
  }
}

/**
 * Implements hook_filter_info().
 *
 * Provide PHP code filter. Use with care.
 */
function php_filter_info() {
  $filters['php_code'] = array(
    'title' => t('PHP evaluator'),
    'description' => t('Executes a piece of PHP code. The usage of this filter should be restricted to administrators only!'),
    'process callback' => 'php_eval',
    'tips callback' => '_php_filter_tips',
    'cache' => FALSE,
  );
  return $filters;
}

