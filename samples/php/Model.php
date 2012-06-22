<?php
/**
 * Object-relational mapper.
 *
 * DBO-backed object data model, for mapping database tables to Cake objects.
 *
 * PHP versions 5
 *
 * CakePHP(tm) : Rapid Development Framework (http://cakephp.org)
 * Copyright 2005-2012, Cake Software Foundation, Inc. (http://cakefoundation.org)
 *
 * Licensed under The MIT License
 * Redistributions of files must retain the above copyright notice.
 *
 * @copyright     Copyright 2005-2012, Cake Software Foundation, Inc. (http://cakefoundation.org)
 * @link          http://cakephp.org CakePHP(tm) Project
 * @package       Cake.Model
 * @since         CakePHP(tm) v 0.10.0.0
 * @license       MIT License (http://www.opensource.org/licenses/mit-license.php)
 */

App::uses('ClassRegistry', 'Utility');
App::uses('Validation', 'Utility');
App::uses('String', 'Utility');
App::uses('Set', 'Utility');
App::uses('BehaviorCollection', 'Model');
App::uses('ModelBehavior', 'Model');
App::uses('ConnectionManager', 'Model');
App::uses('Xml', 'Utility');
App::uses('CakeEvent', 'Event');
App::uses('CakeEventListener', 'Event');
App::uses('CakeEventManager', 'Event');

/**
 * Object-relational mapper.
 *
 * DBO-backed object data model.
 * Automatically selects a database table name based on a pluralized lowercase object class name
 * (i.e. class 'User' => table 'users'; class 'Man' => table 'men')
 * The table is required to have at least 'id auto_increment' primary key.
 *
 * @package       Cake.Model
 * @link          http://book.cakephp.org/2.0/en/models.html
 */
class Model extends Object implements CakeEventListener {

/**
 * The name of the DataSource connection that this Model uses
 *
 * The value must be an attribute name that you defined in `app/Config/database.php`
 * or created using `ConnectionManager::create()`.
 *
 * @var string
 * @link http://book.cakephp.org/2.0/en/models/model-attributes.html#usedbconfig
 */
	public $useDbConfig = 'default';

/**
 * Custom database table name, or null/false if no table association is desired.
 *
 * @var string
 * @link http://book.cakephp.org/2.0/en/models/model-attributes.html#useTable
 */
	public $useTable = null;

/**
 * Custom display field name. Display fields are used by Scaffold, in SELECT boxes' OPTION elements.
 *
 * This field is also used in `find('list')` when called with no extra parameters in the fields list
 *
 * @var string
 * @link http://book.cakephp.org/2.0/en/models/model-attributes.html#displayField
 */
	public $displayField = null;

/**
 * Value of the primary key ID of the record that this model is currently pointing to.
 * Automatically set after database insertions.
 *
 * @var mixed
 */
	public $id = false;

/**
 * Container for the data that this model gets from persistent storage (usually, a database).
 *
 * @var array
 * @link http://book.cakephp.org/2.0/en/models/model-attributes.html#data
 */
	public $data = array();

/**
 * Holds physical schema/database name for this model.  Automatically set during Model creation.
 *
 * @var string
 * @access public
 */
	public $schemaName = null;

/**
 * Table name for this Model.
 *
 * @var string
 */
	public $table = false;

/**
 * The name of the primary key field for this model.
 *
 * @var string
 * @link http://book.cakephp.org/2.0/en/models/model-attributes.html#primaryKey
 */
	public $primaryKey = null;

/**
 * Field-by-field table metadata.
 *
 * @var array
 */
	protected $_schema = null;

/**
 * List of validation rules. It must be an array with the field name as key and using
 * as value one of the following possibilities
 *
 * ### Validating using regular expressions
 *
 * {{{
 * public $validate = array(
 *     'name' => '/^[a-z].+$/i'
 * );
 * }}}
 *
 * ### Validating using methods (no parameters)
 *
 * {{{
 * public $validate = array(
 *     'name' => 'notEmpty'
 * );
 * }}}
 *
 * ### Validating using methods (with parameters)
 *
 * {{{
 * public $validate = array(
 *     'age' => array(
 *         'rule' => array('between', 5, 25)
 *     )
 * );
 * }}}
 *
 * ### Validating using custom method
 *
 * {{{
 * public $validate = array(
 *     'password' => array(
 *         'rule' => array('customValidation')
 *     )
 * );
 * public function customValidation($data) {
 *     // $data will contain array('password' => 'value')
 *     if (isset($this->data[$this->alias]['password2'])) {
 *         return $this->data[$this->alias]['password2'] === current($data);
 *     }
 *     return true;
 * }
 * }}}
 *
 * ### Validations with messages
 *
 * The messages will be used in Model::$validationErrors and can be used in the FormHelper
 *
 * {{{
 * public $validate = array(
 *     'age' => array(
 *         'rule' => array('between', 5, 25),
 *         'message' => array('The age must be between %d and %d.')
 *     )
 * );
 * }}}
 *
 * ### Multiple validations to the same field
 *
 * {{{
 * public $validate = array(
 *     'login' => array(
 *         array(
 *             'rule' => 'alphaNumeric',
 *             'message' => 'Only alphabets and numbers allowed',
 *             'last' => true
 *         ),
 *         array(
 *             'rule' => array('minLength', 8),
 *             'message' => array('Minimum length of %d characters')
 *         )
 *     )
 * );
 * }}}
 *
 * ### Valid keys in validations
 *
 * - `rule`: String with method name, regular expression (started by slash) or array with method and parameters
 * - `message`: String with the message or array if have multiple parameters. See http://php.net/sprintf
 * - `last`: Boolean value to indicate if continue validating the others rules if the current fail [Default: true]
 * - `required`: Boolean value to indicate if the field must be present on save
 * - `allowEmpty`: Boolean value to indicate if the field can be empty
 * - `on`: Possible values: `update`, `create`. Indicate to apply this rule only on update or create
 *
 * @var array
 * @link http://book.cakephp.org/2.0/en/models/model-attributes.html#validate
 * @link http://book.cakephp.org/2.0/en/models/data-validation.html
 */
	public $validate = array();

/**
 * List of validation errors.
 *
 * @var array
 */
	public $validationErrors = array();

/**
 * Name of the validation string domain to use when translating validation errors.
 *
 * @var string
 */
	public $validationDomain = null;

/**
 * Database table prefix for tables in model.
 *
 * @var string
 * @link http://book.cakephp.org/2.0/en/models/model-attributes.html#tableprefix
 */
	public $tablePrefix = null;

/**
 * Name of the model.
 *
 * @var string
 * @link http://book.cakephp.org/2.0/en/models/model-attributes.html#name
 */
	public $name = null;

/**
 * Alias name for model.
 *
 * @var string
 */
	public $alias = null;

/**
 * List of table names included in the model description. Used for associations.
 *
 * @var array
 */
	public $tableToModel = array();

/**
 * Whether or not to cache queries for this model.  This enables in-memory
 * caching only, the results are not stored beyond the current request.
 *
 * @var boolean
 * @link http://book.cakephp.org/2.0/en/models/model-attributes.html#cacheQueries
 */
	public $cacheQueries = false;

/**
 * Detailed list of belongsTo associations.
 *
 * ### Basic usage
 *
 * `public $belongsTo = array('Group', 'Department');`
 *
 * ### Detailed configuration
 *
 * {{{
 * public $belongsTo = array(
 *     'Group',
 *     'Department' => array(
 *         'className' => 'Department',
 *         'foreignKey' => 'department_id'
 *     )
 * );
 * }}}
 *
 * ### Possible keys in association
 *
 * - `className`: the classname of the model being associated to the current model.
 *   If you're defining a 'Profile belongsTo User' relationship, the className key should equal 'User.'
 * - `foreignKey`: the name of the foreign key found in the current model. This is
 *   especially handy if you need to define multiple belongsTo relationships. The default
 *   value for this key is the underscored, singular name of the other model, suffixed with '_id'.
 * - `conditions`: An SQL fragment used to filter related model records. It's good
 *   practice to use model names in SQL fragments: 'User.active = 1' is always
 *   better than just 'active = 1.'
 * - `type`: the type of the join to use in the SQL query, default is LEFT which
 *   may not fit your needs in all situations, INNER may be helpful when you want
 *   everything from your main and associated models or nothing at all!(effective
 *   when used with some conditions of course). (NB: type value is in lower case - i.e. left, inner)
 * - `fields`: A list of fields to be retrieved when the associated model data is
 *   fetched. Returns all fields by default.
 * - `order`: An SQL fragment that defines the sorting order for the returned associated rows.
 * - `counterCache`: If set to true the associated Model will automatically increase or
 *   decrease the "[singular_model_name]_count" field in the foreign table whenever you do
 *   a save() or delete(). If its a string then its the field name to use. The value in the
 *   counter field represents the number of related rows.
 * - `counterScope`: Optional conditions array to use for updating counter cache field.
 *
 * @var array
 * @link http://book.cakephp.org/2.0/en/models/associations-linking-models-together.html#belongsto
 */
	public $belongsTo = array();

/**
 * Detailed list of hasOne associations.
 *
 * ### Basic usage
 *
 * `public $hasOne = array('Profile', 'Address');`
 *
 * ### Detailed configuration
 *
 * {{{
 * public $hasOne = array(
 *     'Profile',
 *     'Address' => array(
 *         'className' => 'Address',
 *         'foreignKey' => 'user_id'
 *     )
 * );
 * }}}
 *
 * ### Possible keys in association
 *
 * - `className`: the classname of the model being associated to the current model.
 *   If you're defining a 'User hasOne Profile' relationship, the className key should equal 'Profile.'
 * - `foreignKey`: the name of the foreign key found in the other model. This is
 *   especially handy if you need to define multiple hasOne relationships.
 *   The default value for this key is the underscored, singular name of the
 *   current model, suffixed with '_id'. In the example above it would default to 'user_id'.
 * - `conditions`: An SQL fragment used to filter related model records. It's good
 *   practice to use model names in SQL fragments: "Profile.approved = 1" is
 *   always better than just "approved = 1."
 * - `fields`: A list of fields to be retrieved when the associated model data is
 *   fetched. Returns all fields by default.
 * - `order`: An SQL fragment that defines the sorting order for the returned associated rows.
 * - `dependent`: When the dependent key is set to true, and the model's delete()
 *   method is called with the cascade parameter set to true, associated model
 *   records are also deleted. In this case we set it true so that deleting a
 *   User will also delete her associated Profile.
 *
 * @var array
 * @link http://book.cakephp.org/2.0/en/models/associations-linking-models-together.html#hasone
 */
	public $hasOne = array();

/**
 * Detailed list of hasMany associations.
 *
 * ### Basic usage
 *
 * `public $hasMany = array('Comment', 'Task');`
 *
 * ### Detailed configuration
 *
 * {{{
 * public $hasMany = array(
 *     'Comment',
 *     'Task' => array(
 *         'className' => 'Task',
 *         'foreignKey' => 'user_id'
 *     )
 * );
 * }}}
 *
 * ### Possible keys in association
 *
 * - `className`: the classname of the model being associated to the current model.
 *   If you're defining a 'User hasMany Comment' relationship, the className key should equal 'Comment.'
 * - `foreignKey`: the name of the foreign key found in the other model. This is
 *   especially handy if you need to define multiple hasMany relationships. The default
 *   value for this key is the underscored, singular name of the actual model, suffixed with '_id'.
 * - `conditions`: An SQL fragment used to filter related model records. It's good
 *   practice to use model names in SQL fragments: "Comment.status = 1" is always
 *   better than just "status = 1."
 * - `fields`: A list of fields to be retrieved when the associated model data is
 *   fetched. Returns all fields by default.
 * - `order`: An SQL fragment that defines the sorting order for the returned associated rows.
 * - `limit`: The maximum number of associated rows you want returned.
 * - `offset`: The number of associated rows to skip over (given the current
 *   conditions and order) before fetching and associating.
 * - `dependent`: When dependent is set to true, recursive model deletion is
 *   possible. In this example, Comment records will be deleted when their
 *   associated User record has been deleted.
 * - `exclusive`: When exclusive is set to true, recursive model deletion does
 *   the delete with a deleteAll() call, instead of deleting each entity separately.
 *   This greatly improves performance, but may not be ideal for all circumstances.
 * - `finderQuery`: A complete SQL query CakePHP can use to fetch associated model
 *   records. This should be used in situations that require very custom results.
 *
 * @var array
 * @link http://book.cakephp.org/2.0/en/models/associations-linking-models-together.html#hasmany
 */
	public $hasMany = array();

/**
 * Detailed list of hasAndBelongsToMany associations.
 *
 * ### Basic usage
 *
 * `public $hasAndBelongsToMany = array('Role', 'Address');`
 *
 * ### Detailed configuration
 *
 * {{{
 * public $hasAndBelongsToMany = array(
 *     'Role',
 *     'Address' => array(
 *         'className' => 'Address',
 *         'foreignKey' => 'user_id',
 *         'associationForeignKey' => 'address_id',
 *         'joinTable' => 'addresses_users'
 *     )
 * );
 * }}}
 *
 * ### Possible keys in association
 *
 * - `className`: the classname of the model being associated to the current model.
 *   If you're defining a 'Recipe HABTM Tag' relationship, the className key should equal 'Tag.'
 * - `joinTable`: The name of the join table used in this association (if the
 *   current table doesn't adhere to the naming convention for HABTM join tables).
 * - `with`: Defines the name of the model for the join table. By default CakePHP
 *   will auto-create a model for you. Using the example above it would be called
 *   RecipesTag. By using this key you can override this default name. The join
 *   table model can be used just like any "regular" model to access the join table directly.
 * - `foreignKey`: the name of the foreign key found in the current model.
 *   This is especially handy if you need to define multiple HABTM relationships.
 *   The default value for this key is the underscored, singular name of the
 *   current model, suffixed with '_id'.
 * - `associationForeignKey`: the name of the foreign key found in the other model.
 *   This is especially handy if you need to define multiple HABTM relationships.
 *   The default value for this key is the underscored, singular name of the other
 *   model, suffixed with '_id'.
 * - `unique`: If true (default value) cake will first delete existing relationship
 *   records in the foreign keys table before inserting new ones, when updating a
 *   record. So existing associations need to be passed again when updating.
 *   To prevent deletion of existing relationship records, set this key to a string 'keepExisting'.
 * - `conditions`: An SQL fragment used to filter related model records. It's good
 *   practice to use model names in SQL fragments: "Comment.status = 1" is always
 *   better than just "status = 1."
 * - `fields`: A list of fields to be retrieved when the associated model data is
 *   fetched. Returns all fields by default.
 * - `order`: An SQL fragment that defines the sorting order for the returned associated rows.
 * - `limit`: The maximum number of associated rows you want returned.
 * - `offset`: The number of associated rows to skip over (given the current
 *   conditions and order) before fetching and associating.
 * - `finderQuery`, `deleteQuery`, `insertQuery`: A complete SQL query CakePHP
 *   can use to fetch, delete, or create new associated model records. This should
 *   be used in situations that require very custom results.
 *
 * @var array
 * @link http://book.cakephp.org/2.0/en/models/associations-linking-models-together.html#hasandbelongstomany-habtm
 */
	public $hasAndBelongsToMany = array();

/**
 * List of behaviors to load when the model object is initialized. Settings can be
 * passed to behaviors by using the behavior name as index. Eg:
 *
 * public $actsAs = array('Translate', 'MyBehavior' => array('setting1' => 'value1'))
 *
 * @var array
 * @link http://book.cakephp.org/2.0/en/models/behaviors.html#using-behaviors
 */
	public $actsAs = null;

/**
 * Holds the Behavior objects currently bound to this model.
 *
 * @var BehaviorCollection
 */
	public $Behaviors = null;

/**
 * Whitelist of fields allowed to be saved.
 *
 * @var array
 */
	public $whitelist = array();

/**
 * Whether or not to cache sources for this model.
 *
 * @var boolean
 */
	public $cacheSources = true;

/**
 * Type of find query currently executing.
 *
 * @var string
 */
	public $findQueryType = null;

/**
 * Number of associations to recurse through during find calls. Fetches only
 * the first level by default.
 *
 * @var integer
 * @link http://book.cakephp.org/2.0/en/models/model-attributes.html#recursive
 */
	public $recursive = 1;

/**
 * The column name(s) and direction(s) to order find results by default.
 *
 * public $order = "Post.created DESC";
 * public $order = array("Post.view_count DESC", "Post.rating DESC");
 *
 * @var string
 * @link http://book.cakephp.org/2.0/en/models/model-attributes.html#order
 */
	public $order = null;

/**
 * Array of virtual fields this model has.  Virtual fields are aliased
 * SQL expressions. Fields added to this property will be read as other fields in a model
 * but will not be saveable.
 *
 * `public $virtualFields = array('two' => '1 + 1');`
 *
 * Is a simplistic example of how to set virtualFields
 *
 * @var array
 * @link http://book.cakephp.org/2.0/en/models/model-attributes.html#virtualfields
 */
	public $virtualFields = array();

/**
 * Default list of association keys.
 *
 * @var array
 */
	protected $_associationKeys = array(
		'belongsTo' => array('className', 'foreignKey', 'conditions', 'fields', 'order', 'counterCache'),
		'hasOne' => array('className', 'foreignKey', 'conditions', 'fields', 'order', 'dependent'),
		'hasMany' => array('className', 'foreignKey', 'conditions', 'fields', 'order', 'limit', 'offset', 'dependent', 'exclusive', 'finderQuery', 'counterQuery'),
		'hasAndBelongsToMany' => array('className', 'joinTable', 'with', 'foreignKey', 'associationForeignKey', 'conditions', 'fields', 'order', 'limit', 'offset', 'unique', 'finderQuery', 'deleteQuery', 'insertQuery')
	);

/**
 * Holds provided/generated association key names and other data for all associations.
 *
 * @var array
 */
	protected $_associations = array('belongsTo', 'hasOne', 'hasMany', 'hasAndBelongsToMany');

/**
 * Holds model associations temporarily to allow for dynamic (un)binding.
 *
 * @var array
 */
	public $__backAssociation = array();

/**
 * Back inner association
 *
 * @var array
 */
	public $__backInnerAssociation = array();

/**
 * Back original association
 *
 * @var array
 */
	public $__backOriginalAssociation = array();

/**
 * Back containable association
 *
 * @var array
 */
	public $__backContainableAssociation = array();

/**
 * The ID of the model record that was last inserted.
 *
 * @var integer
 */
	protected $_insertID = null;

/**
 * Has the datasource been configured.
 *
 * @var boolean
 * @see Model::getDataSource
 */
	protected $_sourceConfigured = false;

/**
 * List of valid finder method options, supplied as the first parameter to find().
 *
 * @var array
 */
	public $findMethods = array(
		'all' => true, 'first' => true, 'count' => true,
		'neighbors' => true, 'list' => true, 'threaded' => true
	);

/**
 * Instance of the CakeEventManager this model is using
 * to dispatch inner events.
 *
 * @var CakeEventManager
 */
	protected $_eventManager = null;

/**
 * Constructor. Binds the model's database table to the object.
 *
 * If `$id` is an array it can be used to pass several options into the model.
 *
 * - id - The id to start the model on.
 * - table - The table to use for this model.
 * - ds - The connection name this model is connected to.
 * - name - The name of the model eg. Post.
 * - alias - The alias of the model, this is used for registering the instance in the `ClassRegistry`.
 *   eg. `ParentThread`
 *
 * ### Overriding Model's __construct method.
 *
 * When overriding Model::__construct() be careful to include and pass in all 3 of the
 * arguments to `parent::__construct($id, $table, $ds);`
 *
 * ### Dynamically creating models
 *
 * You can dynamically create model instances using the $id array syntax.
 *
 * {{{
 * $Post = new Model(array('table' => 'posts', 'name' => 'Post', 'ds' => 'connection2'));
 * }}}
 *
 * Would create a model attached to the posts table on connection2.  Dynamic model creation is useful
 * when you want a model object that contains no associations or attached behaviors.
 *
 * @param mixed $id Set this ID for this model on startup, can also be an array of options, see above.
 * @param string $table Name of database table to use.
 * @param string $ds DataSource connection name.
 */
	public function __construct($id = false, $table = null, $ds = null) {
		parent::__construct();

		if (is_array($id)) {
			extract(array_merge(
				array(
					'id' => $this->id, 'table' => $this->useTable, 'ds' => $this->useDbConfig,
					'name' => $this->name, 'alias' => $this->alias
				),
				$id
			));
		}

		if ($this->name === null) {
			$this->name = (isset($name) ? $name : get_class($this));
		}

		if ($this->alias === null) {
			$this->alias = (isset($alias) ? $alias : $this->name);
		}

		if ($this->primaryKey === null) {
			$this->primaryKey = 'id';
		}

		ClassRegistry::addObject($this->alias, $this);

		$this->id = $id;
		unset($id);

		if ($table === false) {
			$this->useTable = false;
		} elseif ($table) {
			$this->useTable = $table;
		}

		if ($ds !== null) {
			$this->useDbConfig = $ds;
		}

		if (is_subclass_of($this, 'AppModel')) {
			$merge = array('actsAs', 'findMethods');
			$parentClass = get_parent_class($this);
			if ($parentClass !== 'AppModel') {
				$this->_mergeVars($merge, $parentClass);
			}
			$this->_mergeVars($merge, 'AppModel');
		}
		$this->_mergeVars(array('findMethods'), 'Model');

		$this->Behaviors = new BehaviorCollection();

		if ($this->useTable !== false) {

			if ($this->useTable === null) {
				$this->useTable = Inflector::tableize($this->name);
			}

			if ($this->displayField == null) {
				unset($this->displayField);
			}
			$this->table = $this->useTable;
			$this->tableToModel[$this->table] = $this->alias;
		} elseif ($this->table === false) {
			$this->table = Inflector::tableize($this->name);
		}

		if ($this->tablePrefix === null) {
			unset($this->tablePrefix);
		}

		$this->_createLinks();
		$this->Behaviors->init($this->alias, $this->actsAs);
	}

/**
 * Returns a list of all events that will fire in the model during it's lifecycle.
 * You can override this function to add you own listener callbacks
 *
 * @return array
 */
	public function implementedEvents() {
		return array(
			'Model.beforeFind' => array('callable' => 'beforeFind', 'passParams' => true),
			'Model.afterFind' => array('callable' => 'afterFind', 'passParams' => true),
			'Model.beforeValidate' => array('callable' => 'beforeValidate', 'passParams' => true),
			'Model.beforeSave' => array('callable' => 'beforeSave', 'passParams' => true),
			'Model.afterSave' => array('callable' => 'afterSave', 'passParams' => true),
			'Model.beforeDelete' => array('callable' => 'beforeDelete', 'passParams' => true),
			'Model.afterDelete' => array('callable' => 'afterDelete'),
		);
	}

/**
 * Returns the CakeEventManager manager instance that is handling any callbacks.
 * You can use this instance to register any new listeners or callbacks to the
 * model events, or create your own events and trigger them at will.
 *
 * @return CakeEventManager
 */
	public function getEventManager() {
		if (empty($this->_eventManager)) {
			$this->_eventManager = new CakeEventManager();
			$this->_eventManager->attach($this->Behaviors);
			$this->_eventManager->attach($this);
		}
		return $this->_eventManager;
	}

/**
 * Handles custom method calls, like findBy<field> for DB models,
 * and custom RPC calls for remote data sources.
 *
 * @param string $method Name of method to call.
 * @param array $params Parameters for the method.
 * @return mixed Whatever is returned by called method
 */
	public function __call($method, $params) {
		$result = $this->Behaviors->dispatchMethod($this, $method, $params);
		if ($result !== array('unhandled')) {
			return $result;
		}
		$return = $this->getDataSource()->query($method, $params, $this);
		return $return;
	}

/**
 * Handles the lazy loading of model associations by looking in the association arrays for the requested variable
 *
 * @param string $name variable tested for existence in class
 * @return boolean true if the variable exists (if is a not loaded model association it will be created), false otherwise
 */
	public function __isset($name) {
		$className = false;

		foreach ($this->_associations as $type) {
			if (isset($name, $this->{$type}[$name])) {
				$className = empty($this->{$type}[$name]['className']) ? $name : $this->{$type}[$name]['className'];
				break;
			} elseif (isset($name, $this->__backAssociation[$type][$name])) {
				$className = empty($this->__backAssociation[$type][$name]['className']) ?
					$name : $this->__backAssociation[$type][$name]['className'];
				break;
			} elseif ($type == 'hasAndBelongsToMany') {
				foreach ($this->{$type} as $k => $relation) {
					if (empty($relation['with'])) {
						continue;
					}
					if (is_array($relation['with'])) {
						if (key($relation['with']) === $name) {
							$className = $name;
						}
					} else {
						list($plugin, $class) = pluginSplit($relation['with']);
						if ($class === $name) {
							$className = $relation['with'];
						}
					}
					if ($className) {
						$assocKey = $k;
						$dynamic = !empty($relation['dynamicWith']);
						break(2);
					}
				}
			}
		}

		if (!$className) {
			return false;
		}

		list($plugin, $className) = pluginSplit($className);

		if (!ClassRegistry::isKeySet($className) && !empty($dynamic)) {
			$this->{$className} = new AppModel(array(
				'name' => $className,
				'table' => $this->hasAndBelongsToMany[$assocKey]['joinTable'],
				'ds' => $this->useDbConfig
			));
		} else {
			$this->_constructLinkedModel($name, $className, $plugin);
		}

		if (!empty($assocKey)) {
			$this->hasAndBelongsToMany[$assocKey]['joinTable'] = $this->{$name}->table;
			if (count($this->{$name}->schema()) <= 2 && $this->{$name}->primaryKey !== false) {
				$this->{$name}->primaryKey = $this->hasAndBelongsToMany[$assocKey]['foreignKey'];
			}
		}

		return true;
	}

/**
 * Returns the value of the requested variable if it can be set by __isset()
 *
 * @param string $name variable requested for it's value or reference
 * @return mixed value of requested variable if it is set
 */
	public function __get($name) {
		if ($name === 'displayField') {
			return $this->displayField = $this->hasField(array('title', 'name', $this->primaryKey));
		}
		if ($name === 'tablePrefix') {
			$this->setDataSource();
			if (property_exists($this, 'tablePrefix') && !empty($this->tablePrefix)) {
				return $this->tablePrefix;
			}
			return $this->tablePrefix = null;
		}
		if (isset($this->{$name})) {
			return $this->{$name};
		}
	}

/**
 * Bind model associations on the fly.
 *
 * If `$reset` is false, association will not be reset
 * to the originals defined in the model
 *
 * Example: Add a new hasOne binding to the Profile model not
 * defined in the model source code:
 *
 * `$this->User->bindModel( array('hasOne' => array('Profile')) );`
 *
 * Bindings that are not made permanent will be reset by the next Model::find() call on this
 * model.
 *
 * @param array $params Set of bindings (indexed by binding type)
 * @param boolean $reset Set to false to make the binding permanent
 * @return boolean Success
 * @link http://book.cakephp.org/2.0/en/models/associations-linking-models-together.html#creating-and-destroying-associations-on-the-fly
 */
	public function bindModel($params, $reset = true) {
		foreach ($params as $assoc => $model) {
			if ($reset === true && !isset($this->__backAssociation[$assoc])) {
				$this->__backAssociation[$assoc] = $this->{$assoc};
			}
			foreach ($model as $key => $value) {
				$assocName = $key;

				if (is_numeric($key)) {
					$assocName = $value;
					$value = array();
				}
				$this->{$assoc}[$assocName] = $value;
				if (property_exists($this, $assocName)) {
					unset($this->{$assocName});
				}
				if ($reset === false && isset($this->__backAssociation[$assoc])) {
					$this->__backAssociation[$assoc][$assocName] = $value;
				}
			}
		}
		$this->_createLinks();
		return true;
	}

/**
 * Turn off associations on the fly.
 *
 * If $reset is false, association will not be reset
 * to the originals defined in the model
 *
 * Example: Turn off the associated Model Support request,
 * to temporarily lighten the User model:
 *
 * `$this->User->unbindModel( array('hasMany' => array('Supportrequest')) );`
 *
 * unbound models that are not made permanent will reset with the next call to Model::find()
 *
 * @param array $params Set of bindings to unbind (indexed by binding type)
 * @param boolean $reset  Set to false to make the unbinding permanent
 * @return boolean Success
 * @link http://book.cakephp.org/2.0/en/models/associations-linking-models-together.html#creating-and-destroying-associations-on-the-fly
 */
	public function unbindModel($params, $reset = true) {
		foreach ($params as $assoc => $models) {
			if ($reset === true && !isset($this->__backAssociation[$assoc])) {
				$this->__backAssociation[$assoc] = $this->{$assoc};
			}
			foreach ($models as $model) {
				if ($reset === false && isset($this->__backAssociation[$assoc][$model])) {
					unset($this->__backAssociation[$assoc][$model]);
				}
				unset($this->{$assoc}[$model]);
			}
		}
		return true;
	}

/**
 * Create a set of associations.
 *
 * @return void
 */
	protected function _createLinks() {
		foreach ($this->_associations as $type) {
			if (!is_array($this->{$type})) {
				$this->{$type} = explode(',', $this->{$type});

				foreach ($this->{$type} as $i => $className) {
					$className = trim($className);
					unset ($this->{$type}[$i]);
					$this->{$type}[$className] = array();
				}
			}

			if (!empty($this->{$type})) {
				foreach ($this->{$type} as $assoc => $value) {
					$plugin = null;

					if (is_numeric($assoc)) {
						unset ($this->{$type}[$assoc]);
						$assoc = $value;
						$value = array();

						if (strpos($assoc, '.') !== false) {
							list($plugin, $assoc) = pluginSplit($assoc);
							$this->{$type}[$assoc] = array('className' => $plugin . '.' . $assoc);
						} else {
							$this->{$type}[$assoc] = $value;
						}
					}
					$this->_generateAssociation($type, $assoc);
				}
			}
		}
	}

/**
 * Protected helper method to create associated models of a given class.
 *
 * @param string $assoc Association name
 * @param string $className Class name
 * @param string $plugin name of the plugin where $className is located
 * 	examples: public $hasMany = array('Assoc' => array('className' => 'ModelName'));
 * 					usage: $this->Assoc->modelMethods();
 *
 * 				public $hasMany = array('ModelName');
 * 					usage: $this->ModelName->modelMethods();
 * @return void
 */
	protected function _constructLinkedModel($assoc, $className = null, $plugin = null) {
		if (empty($className)) {
			$className = $assoc;
		}

		if (!isset($this->{$assoc}) || $this->{$assoc}->name !== $className) {
			if ($plugin) {
				$plugin .= '.';
			}
			$model = array('class' => $plugin . $className, 'alias' => $assoc);
			$this->{$assoc} = ClassRegistry::init($model);
			if ($plugin) {
				ClassRegistry::addObject($plugin . $className, $this->{$assoc});
			}
			if ($assoc) {
				$this->tableToModel[$this->{$assoc}->table] = $assoc;
			}
		}
	}

/**
 * Build an array-based association from string.
 *
 * @param string $type 'belongsTo', 'hasOne', 'hasMany', 'hasAndBelongsToMany'
 * @param string $assocKey
 * @return void
 */
	protected function _generateAssociation($type, $assocKey) {
		$class = $assocKey;
		$dynamicWith = false;

		foreach ($this->_associationKeys[$type] as $key) {

			if (!isset($this->{$type}[$assocKey][$key]) || $this->{$type}[$assocKey][$key] === null) {
				$data = '';

				switch ($key) {
					case 'fields':
						$data = '';
					break;

					case 'foreignKey':
						$data = (($type == 'belongsTo') ? Inflector::underscore($assocKey) : Inflector::singularize($this->table)) . '_id';
					break;

					case 'associationForeignKey':
						$data = Inflector::singularize($this->{$class}->table) . '_id';
					break;

					case 'with':
						$data = Inflector::camelize(Inflector::singularize($this->{$type}[$assocKey]['joinTable']));
						$dynamicWith = true;
					break;

					case 'joinTable':
						$tables = array($this->table, $this->{$class}->table);
						sort ($tables);
						$data = $tables[0] . '_' . $tables[1];
					break;

					case 'className':
						$data = $class;
					break;

					case 'unique':
						$data = true;
					break;
				}
				$this->{$type}[$assocKey][$key] = $data;
			}

			if ($dynamicWith) {
				$this->{$type}[$assocKey]['dynamicWith'] = true;
			}

		}
	}

/**
 * Sets a custom table for your controller class. Used by your controller to select a database table.
 *
 * @param string $tableName Name of the custom table
 * @throws MissingTableException when database table $tableName is not found on data source
 * @return void
 */
	public function setSource($tableName) {
		$this->setDataSource($this->useDbConfig);
		$db = ConnectionManager::getDataSource($this->useDbConfig);
		$db->cacheSources = ($this->cacheSources && $db->cacheSources);

		if (method_exists($db, 'listSources')) {
			$sources = $db->listSources();
			if (is_array($sources) && !in_array(strtolower($this->tablePrefix . $tableName), array_map('strtolower', $sources))) {
				throw new MissingTableException(array(
					'table' => $this->tablePrefix . $tableName,
					'class' => $this->alias,
					'ds' => $this->useDbConfig,
				));
			}
			$this->_schema = null;
		}
		$this->table = $this->useTable = $tableName;
		$this->tableToModel[$this->table] = $this->alias;
	}

/**
 * This function does two things:
 *
 * 1. it scans the array $one for the primary key,
 * and if that's found, it sets the current id to the value of $one[id].
 * For all other keys than 'id' the keys and values of $one are copied to the 'data' property of this object.
 * 2. Returns an array with all of $one's keys and values.
 * (Alternative indata: two strings, which are mangled to
 * a one-item, two-dimensional array using $one for a key and $two as its value.)
 *
 * @param mixed $one Array or string of data
 * @param string $two Value string for the alternative indata method
 * @return array Data with all of $one's keys and values
 * @link http://book.cakephp.org/2.0/en/models/saving-your-data.html
 */
	public function set($one, $two = null) {
		if (!$one) {
			return;
		}
		if (is_object($one)) {
			if ($one instanceof SimpleXMLElement || $one instanceof DOMNode) {
				$one = $this->_normalizeXmlData(Xml::toArray($one));
			} else {
				$one = Set::reverse($one);
			}
		}

		if (is_array($one)) {
			$data = $one;
			if (empty($one[$this->alias])) {
				$data = $this->_setAliasData($one);
			}
		} else {
			$data = array($this->alias => array($one => $two));
		}

		foreach ($data as $modelName => $fieldSet) {
			if (is_array($fieldSet)) {

				foreach ($fieldSet as $fieldName => $fieldValue) {
					if (isset($this->validationErrors[$fieldName])) {
						unset ($this->validationErrors[$fieldName]);
					}

					if ($modelName === $this->alias) {
						if ($fieldName === $this->primaryKey) {
							$this->id = $fieldValue;
						}
					}
					if (is_array($fieldValue) || is_object($fieldValue)) {
						$fieldValue = $this->deconstruct($fieldName, $fieldValue);
					}
					$this->data[$modelName][$fieldName] = $fieldValue;
				}
			}
		}
		return $data;
	}

/**
 * Move values to alias
 *
 * @param array $data
 * @return array
 */
	protected function _setAliasData($data) {
		$models = array_keys($this->getAssociated());
		$schema = array_keys($this->schema());
		foreach ($data as $field => $value) {
			if (in_array($field, $schema) || !in_array($field, $models)) {
				$data[$this->alias][$field] = $value;
				unset($data[$field]);
			}
		}
		return $data;
	}

/**
 * Normalize Xml::toArray() to use in Model::save()
 *
 * @param array $xml XML as array
 * @return array
 */
	protected function _normalizeXmlData(array $xml) {
		$return = array();
		foreach ($xml as $key => $value) {
			if (is_array($value)) {
				$return[Inflector::camelize($key)] = $this->_normalizeXmlData($value);
			} elseif ($key[0] === '@') {
				$return[substr($key, 1)] = $value;
			} else {
				$return[$key] = $value;
			}
		}
		return $return;
	}

/**
 * Deconstructs a complex data type (array or object) into a single field value.
 *
 * @param string $field The name of the field to be deconstructed
 * @param mixed $data An array or object to be deconstructed into a field
 * @return mixed The resulting data that should be assigned to a field
 */
	public function deconstruct($field, $data) {
		if (!is_array($data)) {
			return $data;
		}

		$type = $this->getColumnType($field);

		if (in_array($type, array('datetime', 'timestamp', 'date', 'time'))) {
			$useNewDate = (isset($data['year']) || isset($data['month']) ||
				isset($data['day']) || isset($data['hour']) || isset($data['minute']));

			$dateFields = array('Y' => 'year', 'm' => 'month', 'd' => 'day', 'H' => 'hour', 'i' => 'min', 's' => 'sec');
			$timeFields = array('H' => 'hour', 'i' => 'min', 's' => 'sec');
			$date = array();

			if (isset($data['meridian']) && empty($data['meridian'])) {
				return null;
			}

			if (
				isset($data['hour']) &&
				isset($data['meridian']) &&
				!empty($data['hour']) &&
				$data['hour'] != 12 &&
				'pm' == $data['meridian']
			) {
				$data['hour'] = $data['hour'] + 12;
			}
			if (isset($data['hour']) && isset($data['meridian']) && $data['hour'] == 12 && 'am' == $data['meridian']) {
				$data['hour'] = '00';
			}
			if ($type == 'time') {
				foreach ($timeFields as $key => $val) {
					if (!isset($data[$val]) || $data[$val] === '0' || $data[$val] === '00') {
						$data[$val] = '00';
					} elseif ($data[$val] !== '') {
						$data[$val] = sprintf('%02d', $data[$val]);
					}
					if (!empty($data[$val])) {
						$date[$key] = $data[$val];
					} else {
						return null;
					}
				}
			}

			if ($type == 'datetime' || $type == 'timestamp' || $type == 'date') {
				foreach ($dateFields as $key => $val) {
					if ($val == 'hour' || $val == 'min' || $val == 'sec') {
						if (!isset($data[$val]) || $data[$val] === '0' || $data[$val] === '00') {
							$data[$val] = '00';
						} else {
							$data[$val] = sprintf('%02d', $data[$val]);
						}
					}
					if (!isset($data[$val]) || isset($data[$val]) && (empty($data[$val]) || $data[$val][0] === '-')) {
						return null;
					}
					if (isset($data[$val]) && !empty($data[$val])) {
						$date[$key] = $data[$val];
					}
				}
			}

			if ($useNewDate && !empty($date)) {
				$format = $this->getDataSource()->columns[$type]['format'];
				foreach (array('m', 'd', 'H', 'i', 's') as $index) {
					if (isset($date[$index])) {
						$date[$index] = sprintf('%02d', $date[$index]);
					}
				}
				return str_replace(array_keys($date), array_values($date), $format);
			}
		}
		return $data;
	}

/**
 * Returns an array of table metadata (column names and types) from the database.
 * $field => keys(type, null, default, key, length, extra)
 *
 * @param mixed $field Set to true to reload schema, or a string to return a specific field
 * @return array Array of table metadata
 */
	public function schema($field = false) {
		if ($this->useTable !== false && (!is_array($this->_schema) || $field === true)) {
			$db = $this->getDataSource();
			$db->cacheSources = ($this->cacheSources && $db->cacheSources);
			if (method_exists($db, 'describe') && $this->useTable !== false) {
				$this->_schema = $db->describe($this);
			} elseif ($this->useTable === false) {
				$this->_schema = array();
			}
		}
		if (is_string($field)) {
			if (isset($this->_schema[$field])) {
				return $this->_schema[$field];
			} else {
				return null;
			}
		}
		return $this->_schema;
	}

/**
 * Returns an associative array of field names and column types.
 *
 * @return array Field types indexed by field name
 */
	public function getColumnTypes() {
		$columns = $this->schema();
		if (empty($columns)) {
			trigger_error(__d('cake_dev', '(Model::getColumnTypes) Unable to build model field data. If you are using a model without a database table, try implementing schema()'), E_USER_WARNING);
		}
		$cols = array();
		foreach ($columns as $field => $values) {
			$cols[$field] = $values['type'];
		}
		return $cols;
	}

/**
 * Returns the column type of a column in the model.
 *
 * @param string $column The name of the model column
 * @return string Column type
 */
	public function getColumnType($column) {
		$db = $this->getDataSource();
		$cols = $this->schema();
		$model = null;

		$startQuote = isset($db->startQuote) ? $db->startQuote : null;
		$endQuote = isset($db->endQuote) ? $db->endQuote : null;
		$column = str_replace(array($startQuote, $endQuote), '', $column);

		if (strpos($column, '.')) {
			list($model, $column) = explode('.', $column);
		}
		if ($model != $this->alias && isset($this->{$model})) {
			return $this->{$model}->getColumnType($column);
		}
		if (isset($cols[$column]) && isset($cols[$column]['type'])) {
			return $cols[$column]['type'];
		}
		return null;
	}

/**
 * Returns true if the supplied field exists in the model's database table.
 *
 * @param mixed $name Name of field to look for, or an array of names
 * @param boolean $checkVirtual checks if the field is declared as virtual
 * @return mixed If $name is a string, returns a boolean indicating whether the field exists.
 *               If $name is an array of field names, returns the first field that exists,
 *               or false if none exist.
 */
	public function hasField($name, $checkVirtual = false) {
		if (is_array($name)) {
			foreach ($name as $n) {
				if ($this->hasField($n, $checkVirtual)) {
					return $n;
				}
			}
			return false;
		}

		if ($checkVirtual && !empty($this->virtualFields)) {
			if ($this->isVirtualField($name)) {
				return true;
			}
		}

		if (empty($this->_schema)) {
			$this->schema();
		}

		if ($this->_schema != null) {
			return isset($this->_schema[$name]);
		}
		return false;
	}

/**
 * Check that a method is callable on a model.  This will check both the model's own methods, its
 * inherited methods and methods that could be callable through behaviors.
 *
 * @param string $method The method to be called.
 * @return boolean True on method being callable.
 */
	public function hasMethod($method) {
		if (method_exists($this, $method)) {
			return true;
		}
		if ($this->Behaviors->hasMethod($method)) {
			return true;
		}
		return false;
	}

/**
 * Returns true if the supplied field is a model Virtual Field
 *
 * @param string $field Name of field to look for
 * @return boolean indicating whether the field exists as a model virtual field.
 */
	public function isVirtualField($field) {
		if (empty($this->virtualFields) || !is_string($field)) {
			return false;
		}
		if (isset($this->virtualFields[$field])) {
			return true;
		}
		if (strpos($field, '.') !== false) {
			list($model, $field) = explode('.', $field);
			if ($model == $this->alias && isset($this->virtualFields[$field])) {
				return true;
			}
		}
		return false;
	}

/**
 * Returns the expression for a model virtual field
 *
 * @param string $field Name of field to look for
 * @return mixed If $field is string expression bound to virtual field $field
 *    If $field is null, returns an array of all model virtual fields
 *    or false if none $field exist.
 */
	public function getVirtualField($field = null) {
		if ($field == null) {
			return empty($this->virtualFields) ? false : $this->virtualFields;
		}
		if ($this->isVirtualField($field)) {
			if (strpos($field, '.') !== false) {
				list($model, $field) = explode('.', $field);
			}
			return $this->virtualFields[$field];
		}
		return false;
	}

/**
 * Initializes the model for writing a new record, loading the default values
 * for those fields that are not defined in $data, and clearing previous validation errors.
 * Especially helpful for saving data in loops.
 *
 * @param mixed $data Optional data array to assign to the model after it is created.  If null or false,
 *   schema data defaults are not merged.
 * @param boolean $filterKey If true, overwrites any primary key input with an empty value
 * @return array The current Model::data; after merging $data and/or defaults from database
 * @link http://book.cakephp.org/2.0/en/models/saving-your-data.html#model-create-array-data-array
 */
	public function create($data = array(), $filterKey = false) {
		$defaults = array();
		$this->id = false;
		$this->data = array();
		$this->validationErrors = array();

		if ($data !== null && $data !== false) {
			foreach ($this->schema() as $field => $properties) {
				if ($this->primaryKey !== $field && isset($properties['default']) && $properties['default'] !== '') {
					$defaults[$field] = $properties['default'];
				}
			}
			$this->set($defaults);
			$this->set($data);
		}
		if ($filterKey) {
			$this->set($this->primaryKey, false);
		}
		return $this->data;
	}

/**
 * Returns a list of fields from the database, and sets the current model
 * data (Model::$data) with the record found.
 *
 * @param mixed $fields String of single field name, or an array of field names.
 * @param mixed $id The ID of the record to read
 * @return array Array of database fields, or false if not found
 * @link http://book.cakephp.org/2.0/en/models/retrieving-your-data.html#model-read
 */
	public function read($fields = null, $id = null) {
		$this->validationErrors = array();

		if ($id != null) {
			$this->id = $id;
		}

		$id = $this->id;

		if (is_array($this->id)) {
			$id = $this->id[0];
		}

		if ($id !== null && $id !== false) {
			$this->data = $this->find('first', array(
				'conditions' => array($this->alias . '.' . $this->primaryKey => $id),
				'fields' => $fields
			));
			return $this->data;
		} else {
			return false;
		}
	}

/**
 * Returns the contents of a single field given the supplied conditions, in the
 * supplied order.
 *
 * @param string $name Name of field to get
 * @param array $conditions SQL conditions (defaults to NULL)
 * @param string $order SQL ORDER BY fragment
 * @return string field contents, or false if not found
 * @link http://book.cakephp.org/2.0/en/models/retrieving-your-data.html#model-field
 */
	public function field($name, $conditions = null, $order = null) {
		if ($conditions === null && $this->id !== false) {
			$conditions = array($this->alias . '.' . $this->primaryKey => $this->id);
		}
		if ($this->recursive >= 1) {
			$recursive = -1;
		} else {
			$recursive = $this->recursive;
		}
		$fields = $name;
		if ($data = $this->find('first', compact('conditions', 'fields', 'order', 'recursive'))) {
			if (strpos($name, '.') === false) {
				if (isset($data[$this->alias][$name])) {
					return $data[$this->alias][$name];
				}
			} else {
				$name = explode('.', $name);
				if (isset($data[$name[0]][$name[1]])) {
					return $data[$name[0]][$name[1]];
				}
			}
			if (isset($data[0]) && count($data[0]) > 0) {
				return array_shift($data[0]);
			}
		} else {
			return false;
		}
	}

/**
 * Saves the value of a single field to the database, based on the current
 * model ID.
 *
 * @param string $name Name of the table field
 * @param mixed $value Value of the field
 * @param array $validate See $options param in Model::save(). Does not respect 'fieldList' key if passed
 * @return boolean See Model::save()
 * @see Model::save()
 * @link http://book.cakephp.org/2.0/en/models/saving-your-data.html#model-savefield-string-fieldname-string-fieldvalue-validate-false
 */
	public function saveField($name, $value, $validate = false) {
		$id = $this->id;
		$this->create(false);

		if (is_array($validate)) {
			$options = array_merge(array('validate' => false, 'fieldList' => array($name)), $validate);
		} else {
			$options = array('validate' => $validate, 'fieldList' => array($name));
		}
		return $this->save(array($this->alias => array($this->primaryKey => $id, $name => $value)), $options);
	}

/**
 * Saves model data (based on white-list, if supplied) to the database. By
 * default, validation occurs before save.
 *
 * @param array $data Data to save.
 * @param mixed $validate Either a boolean, or an array.
 *   If a boolean, indicates whether or not to validate before saving.
 *   If an array, allows control of validate, callbacks, and fieldList
 * @param array $fieldList List of fields to allow to be written
 * @return mixed On success Model::$data if its not empty or true, false on failure
 * @link http://book.cakephp.org/2.0/en/models/saving-your-data.html
 */
	public function save($data = null, $validate = true, $fieldList = array()) {
		$defaults = array('validate' => true, 'fieldList' => array(), 'callbacks' => true);
		$_whitelist = $this->whitelist;
		$fields = array();

		if (!is_array($validate)) {
			$options = array_merge($defaults, compact('validate', 'fieldList', 'callbacks'));
		} else {
			$options = array_merge($defaults, $validate);
		}

		if (!empty($options['fieldList'])) {
			if (!empty($options['fieldList'][$this->alias]) && is_array($options['fieldList'][$this->alias])) {
				$this->whitelist = $options['fieldList'][$this->alias];
			} else {
				$this->whitelist = $options['fieldList'];
			}
		} elseif ($options['fieldList'] === null) {
			$this->whitelist = array();
		}
		$this->set($data);

		if (empty($this->data) && !$this->hasField(array('created', 'updated', 'modified'))) {
			return false;
		}

		foreach (array('created', 'updated', 'modified') as $field) {
			$keyPresentAndEmpty = (
				isset($this->data[$this->alias]) &&
				array_key_exists($field, $this->data[$this->alias]) &&
				$this->data[$this->alias][$field] === null
			);
			if ($keyPresentAndEmpty) {
				unset($this->data[$this->alias][$field]);
			}
		}

		$exists = $this->exists();
		$dateFields = array('modified', 'updated');

		if (!$exists) {
			$dateFields[] = 'created';
		}
		if (isset($this->data[$this->alias])) {
			$fields = array_keys($this->data[$this->alias]);
		}
		if ($options['validate'] && !$this->validates($options)) {
			$this->whitelist = $_whitelist;
			return false;
		}

		$db = $this->getDataSource();

		foreach ($dateFields as $updateCol) {
			if ($this->hasField($updateCol) && !in_array($updateCol, $fields)) {
				$default = array('formatter' => 'date');
				$colType = array_merge($default, $db->columns[$this->getColumnType($updateCol)]);
				if (!array_key_exists('format', $colType)) {
					$time = strtotime('now');
				} else {
					$time = call_user_func($colType['formatter'], $colType['format']);
				}
				if (!empty($this->whitelist)) {
					$this->whitelist[] = $updateCol;
				}
				$this->set($updateCol, $time);
			}
		}

		if ($options['callbacks'] === true || $options['callbacks'] === 'before') {
			$event = new CakeEvent('Model.beforeSave', $this, array($options));
			list($event->break, $event->breakOn) = array(true, array(false, null));
			$this->getEventManager()->dispatch($event);
			if (!$event->result) {
				$this->whitelist = $_whitelist;
				return false;
			}
		}

		if (empty($this->data[$this->alias][$this->primaryKey])) {
			unset($this->data[$this->alias][$this->primaryKey]);
		}
		$fields = $values = array();

		foreach ($this->data as $n => $v) {
			if (isset($this->hasAndBelongsToMany[$n])) {
				if (isset($v[$n])) {
					$v = $v[$n];
				}
				$joined[$n] = $v;
			} else {
				if ($n === $this->alias) {
					foreach (array('created', 'updated', 'modified') as $field) {
						if (array_key_exists($field, $v) && empty($v[$field])) {
							unset($v[$field]);
						}
					}

					foreach ($v as $x => $y) {
						if ($this->hasField($x) && (empty($this->whitelist) || in_array($x, $this->whitelist))) {
							list($fields[], $values[]) = array($x, $y);
						}
					}
				}
			}
		}
		$count = count($fields);

		if (!$exists && $count > 0) {
			$this->id = false;
		}
		$success = true;
		$created = false;

		if ($count > 0) {
			$cache = $this->_prepareUpdateFields(array_combine($fields, $values));

			if (!empty($this->id)) {
				$success = (bool)$db->update($this, $fields, $values);
			} else {
				$fInfo = $this->schema($this->primaryKey);
				$isUUID = ($fInfo['length'] == 36 &&
					($fInfo['type'] === 'string' || $fInfo['type'] === 'binary')
				);
				if (empty($this->data[$this->alias][$this->primaryKey]) && $isUUID) {
					if (array_key_exists($this->primaryKey, $this->data[$this->alias])) {
						$j = array_search($this->primaryKey, $fields);
						$values[$j] = String::uuid();
					} else {
						list($fields[], $values[]) = array($this->primaryKey, String::uuid());
					}
				}

				if (!$db->create($this, $fields, $values)) {
					$success = $created = false;
				} else {
					$created = true;
				}
			}

			if ($success && !empty($this->belongsTo)) {
				$this->updateCounterCache($cache, $created);
			}
		}

		if (!empty($joined) && $success === true) {
			$this->_saveMulti($joined, $this->id, $db);
		}

		if ($success && $count > 0) {
			if (!empty($this->data)) {
				$success = $this->data;
				if ($created) {
					$this->data[$this->alias][$this->primaryKey] = $this->id;
				}
			}
			if ($options['callbacks'] === true || $options['callbacks'] === 'after') {
				$event = new CakeEvent('Model.afterSave', $this, array($created, $options));
				$this->getEventManager()->dispatch($event);
			}
			if (!empty($this->data)) {
				$success = Set::merge($success, $this->data);
			}
			$this->data = false;
			$this->_clearCache();
			$this->validationErrors = array();
		}
		$this->whitelist = $_whitelist;
		return $success;
	}

/**
 * Saves model hasAndBelongsToMany data to the database.
 *
 * @param array $joined Data to save
 * @param mixed $id ID of record in this model
 * @param DataSource $db
 * @return void
 */
	protected function _saveMulti($joined, $id, $db) {
		foreach ($joined as $assoc => $data) {

			if (isset($this->hasAndBelongsToMany[$assoc])) {
				list($join) = $this->joinModel($this->hasAndBelongsToMany[$assoc]['with']);

				$keyInfo = $this->{$join}->schema($this->{$join}->primaryKey);
				if ($with = $this->hasAndBelongsToMany[$assoc]['with']) {
					$withModel = is_array($with) ? key($with) : $with;
					list($pluginName, $withModel) = pluginSplit($withModel);
					$dbMulti = $this->{$withModel}->getDataSource();
				} else {
					$dbMulti = $db;
				}

				$isUUID = !empty($this->{$join}->primaryKey) && (
						$keyInfo['length'] == 36 && (
						$keyInfo['type'] === 'string' ||
						$keyInfo['type'] === 'binary'
					)
				);

				$newData = $newValues = $newJoins = array();
				$primaryAdded = false;

				$fields = array(
					$dbMulti->name($this->hasAndBelongsToMany[$assoc]['foreignKey']),
					$dbMulti->name($this->hasAndBelongsToMany[$assoc]['associationForeignKey'])
				);

				$idField = $db->name($this->{$join}->primaryKey);
				if ($isUUID && !in_array($idField, $fields)) {
					$fields[] = $idField;
					$primaryAdded = true;
				}

				foreach ((array)$data as $row) {
					if ((is_string($row) && (strlen($row) == 36 || strlen($row) == 16)) || is_numeric($row)) {
						$newJoins[] = $row;
						$values = array($id, $row);
						if ($isUUID && $primaryAdded) {
							$values[] = String::uuid();
						}
						$newValues[$row] = $values;
						unset($values);
					} elseif (isset($row[$this->hasAndBelongsToMany[$assoc]['associationForeignKey']])) {
						if (!empty($row[$this->{$join}->primaryKey])) {
							$newJoins[] = $row[$this->hasAndBelongsToMany[$assoc]['associationForeignKey']];
						}
						$newData[] = $row;
					} elseif (isset($row[$join]) && isset($row[$join][$this->hasAndBelongsToMany[$assoc]['associationForeignKey']])) {
						if (!empty($row[$join][$this->{$join}->primaryKey])) {
							$newJoins[] = $row[$join][$this->hasAndBelongsToMany[$assoc]['associationForeignKey']];
						}
						$newData[] = $row[$join];
					}
				}

				$keepExisting = $this->hasAndBelongsToMany[$assoc]['unique'] === 'keepExisting';
				if ($this->hasAndBelongsToMany[$assoc]['unique']) {
					$conditions = array(
						$join . '.' . $this->hasAndBelongsToMany[$assoc]['foreignKey'] => $id
					);
					if (!empty($this->hasAndBelongsToMany[$assoc]['conditions'])) {
						$conditions = array_merge($conditions, (array)$this->hasAndBelongsToMany[$assoc]['conditions']);
					}
					$associationForeignKey = $this->{$join}->alias . '.' . $this->hasAndBelongsToMany[$assoc]['associationForeignKey'];
					$links = $this->{$join}->find('all', array(
						'conditions' => $conditions,
						'recursive' => empty($this->hasAndBelongsToMany[$assoc]['conditions']) ? -1 : 0,
						'fields' => $associationForeignKey,
					));

					$oldLinks = Set::extract($links, "{n}.{$associationForeignKey}");
					if (!empty($oldLinks)) {
						if ($keepExisting && !empty($newJoins)) {
							$conditions[$associationForeignKey] = array_diff($oldLinks, $newJoins);
						} else {
							$conditions[$associationForeignKey] = $oldLinks;
						}
						$dbMulti->delete($this->{$join}, $conditions);
					}
				}

				if (!empty($newData)) {
					foreach ($newData as $data) {
						$data[$this->hasAndBelongsToMany[$assoc]['foreignKey']] = $id;
						if (empty($data[$this->{$join}->primaryKey])) {
							$this->{$join}->create();
						}
						$this->{$join}->save($data);
					}
				}

				if (!empty($newValues)) {
					if ($keepExisting && !empty($links)) {
						foreach ($links as $link) {
							$oldJoin = $link[$join][$this->hasAndBelongsToMany[$assoc]['associationForeignKey']];
							if (! in_array($oldJoin, $newJoins) ) {
								$conditions[$associationForeignKey] = $oldJoin;
								$db->delete($this->{$join}, $conditions);
							} else {
								unset($newValues[$oldJoin]);
							}
						}
						$newValues = array_values($newValues);
					}
					if (!empty($newValues)) {
						$dbMulti->insertMulti($this->{$join}, $fields, $newValues);
					}
				}
			}
		}
	}

/**
 * Updates the counter cache of belongsTo associations after a save or delete operation
 *
 * @param array $keys Optional foreign key data, defaults to the information $this->data
 * @param boolean $created True if a new record was created, otherwise only associations with
 *   'counterScope' defined get updated
 * @return void
 */
	public function updateCounterCache($keys = array(), $created = false) {
		$keys = empty($keys) ? $this->data[$this->alias] : $keys;
		$keys['old'] = isset($keys['old']) ? $keys['old'] : array();

		foreach ($this->belongsTo as $parent => $assoc) {
			if (!empty($assoc['counterCache'])) {
				if (!is_array($assoc['counterCache'])) {
					if (isset($assoc['counterScope'])) {
						$assoc['counterCache'] = array($assoc['counterCache'] => $assoc['counterScope']);
					} else {
						$assoc['counterCache'] = array($assoc['counterCache'] => array());
					}
				}

				$foreignKey = $assoc['foreignKey'];
				$fkQuoted = $this->escapeField($assoc['foreignKey']);

				foreach ($assoc['counterCache'] as $field => $conditions) {
					if (!is_string($field)) {
						$field = Inflector::underscore($this->alias) . '_count';
					}
					if (!$this->{$parent}->hasField($field)) {
						continue;
					}
					if ($conditions === true) {
						$conditions = array();
					} else {
						$conditions = (array)$conditions;
					}

					if (!array_key_exists($foreignKey, $keys)) {
						$keys[$foreignKey] = $this->field($foreignKey);
					}
					$recursive = (empty($conditions) ? -1 : 0);

					if (isset($keys['old'][$foreignKey])) {
						if ($keys['old'][$foreignKey] != $keys[$foreignKey]) {
							$conditions[$fkQuoted] = $keys['old'][$foreignKey];
							$count = intval($this->find('count', compact('conditions', 'recursive')));

							$this->{$parent}->updateAll(
								array($field => $count),
								array($this->{$parent}->escapeField() => $keys['old'][$foreignKey])
							);
						}
					}
					$conditions[$fkQuoted] = $keys[$foreignKey];

					if ($recursive === 0) {
						$conditions = array_merge($conditions, (array)$conditions);
					}
					$count = intval($this->find('count', compact('conditions', 'recursive')));

					$this->{$parent}->updateAll(
						array($field => $count),
						array($this->{$parent}->escapeField() => $keys[$foreignKey])
					);
				}
			}
		}
	}

/**
 * Helper method for Model::updateCounterCache().  Checks the fields to be updated for
 *
 * @param array $data The fields of the record that will be updated
 * @return array Returns updated foreign key values, along with an 'old' key containing the old
 *     values, or empty if no foreign keys are updated.
 */
	protected function _prepareUpdateFields($data) {
		$foreignKeys = array();
		foreach ($this->belongsTo as $assoc => $info) {
			if ($info['counterCache']) {
				$foreignKeys[$assoc] = $info['foreignKey'];
			}
		}
		$included = array_intersect($foreignKeys, array_keys($data));

		if (empty($included) || empty($this->id)) {
			return array();
		}
		$old = $this->find('first', array(
			'conditions' => array($this->alias . '.' . $this->primaryKey => $this->id),
			'fields' => array_values($included),
			'recursive' => -1
		));
		return array_merge($data, array('old' => $old[$this->alias]));
	}

/**
 * Backwards compatible passthrough method for:
 * saveMany(), validateMany(), saveAssociated() and validateAssociated()
 *
 * Saves multiple individual records for a single model; Also works with a single record, as well as
 * all its associated records.
 *
 * #### Options
 *
 * - validate: Set to false to disable validation, true to validate each record before saving,
 *   'first' to validate *all* records before any are saved (default),
 *   or 'only' to only validate the records, but not save them.
 * - atomic: If true (default), will attempt to save all records in a single transaction.
 *   Should be set to false if database/table does not support transactions.
 * - fieldList: Equivalent to the $fieldList parameter in Model::save().
 *   It should be an associate array with model name as key and array of fields as value. Eg.
 *   {{{
 *   array(
 *       'SomeModel' => array('field'),
 *       'AssociatedModel' => array('field', 'otherfield')
 *   )
 *   }}}
 * - deep: see saveMany/saveAssociated
 *
 * @param array $data Record data to save. This can be either a numerically-indexed array (for saving multiple
 *     records of the same type), or an array indexed by association name.
 * @param array $options Options to use when saving record data, See $options above.
 * @return mixed If atomic: True on success, or false on failure.
 *    Otherwise: array similar to the $data array passed, but values are set to true/false
 *    depending on whether each record saved successfully.
 * @link http://book.cakephp.org/2.0/en/models/saving-your-data.html#model-saveassociated-array-data-null-array-options-array
 * @link http://book.cakephp.org/2.0/en/models/saving-your-data.html#model-saveall-array-data-null-array-options-array
 */
	public function saveAll($data = null, $options = array()) {
		$options = array_merge(array('validate' => 'first'), $options);
		if (Set::numeric(array_keys($data))) {
			if ($options['validate'] === 'only') {
				return $this->validateMany($data, $options);
			}
			return $this->saveMany($data, $options);
		}
		if ($options['validate'] === 'only') {
			return $this->validateAssociated($data, $options);
		}
		return $this->saveAssociated($data, $options);
	}

/**
 * Saves multiple individual records for a single model
 *
 * #### Options
 *
 * - validate: Set to false to disable validation, true to validate each record before saving,
 *   'first' to validate *all* records before any are saved (default),
 * - atomic: If true (default), will attempt to save all records in a single transaction.
 *   Should be set to false if database/table does not support transactions.
 * - fieldList: Equivalent to the $fieldList parameter in Model::save()
 * - deep: If set to true, all associated data will be saved as well.
 *
 * @param array $data Record data to save. This should be a numerically-indexed array
 * @param array $options Options to use when saving record data, See $options above.
 * @return mixed If atomic: True on success, or false on failure.
 *    Otherwise: array similar to the $data array passed, but values are set to true/false
 *    depending on whether each record saved successfully.
 * @link http://book.cakephp.org/2.0/en/models/saving-your-data.html#model-savemany-array-data-null-array-options-array
 */
	public function saveMany($data = null, $options = array()) {
		if (empty($data)) {
			$data = $this->data;
		}

		$options = array_merge(array('validate' => 'first', 'atomic' => true, 'deep' => false), $options);
		$this->validationErrors = $validationErrors = array();

		if (empty($data) && $options['validate'] !== false) {
			$result = $this->save($data, $options);
			if (!$options['atomic']) {
				return array(!empty($result));
			}
			return !empty($result);
		}

		if ($options['validate'] === 'first') {
			$validates = $this->validateMany($data, $options);
			if ((!$validates && $options['atomic']) || (!$options['atomic'] && in_array(false, $validates, true))) {
				return $validates;
			}
			$options['validate'] = false;
		}

		if ($options['atomic']) {
			$db = $this->getDataSource();
			$transactionBegun = $db->begin();
		}
		$return = array();
		foreach ($data as $key => $record) {
			$validates = $this->create(null) !== null;
			$saved = false;
			if ($validates) {
				if ($options['deep']) {
					$saved = $this->saveAssociated($record, array_merge($options, array('atomic' => false)));
				} else {
					$saved = $this->save($record, $options);
				}
			}
			$validates = ($validates && ($saved === true || (is_array($saved) && !in_array(false, $saved, true))));
			if (!$validates) {
				$validationErrors[$key] = $this->validationErrors;
			}
			if (!$options['atomic']) {
				$return[$key] = $validates;
			} elseif (!$validates) {
				break;
			}
		}
		$this->validationErrors = $validationErrors;

		if (!$options['atomic']) {
			return $return;
		}
		if ($validates) {
			if ($transactionBegun) {
				return $db->commit() !== false;
			} else {
				return true;
			}
		}
		$db->rollback();
		return false;
	}

/**
 * Validates multiple individual records for a single model
 *
 * #### Options
 *
 * - atomic: If true (default), returns boolean. If false returns array.
 * - fieldList: Equivalent to the $fieldList parameter in Model::save()
 * - deep: If set to true, all associated data will be validated as well.
 *
 * Warning: This method could potentially change the passed argument `$data`,
 * If you do not want this to happen, make a copy of `$data` before passing it
 * to this method
 *
 * @param array $data Record data to validate. This should be a numerically-indexed array
 * @param array $options Options to use when validating record data (see above), See also $options of validates().
 * @return boolean True on success, or false on failure.
 * @return mixed If atomic: True on success, or false on failure.
 *    Otherwise: array similar to the $data array passed, but values are set to true/false
 *    depending on whether each record validated successfully.
 */
	public function validateMany(&$data, $options = array()) {
		$options = array_merge(array('atomic' => true, 'deep' => false), $options);
		$this->validationErrors = $validationErrors = $return = array();
		foreach ($data as $key => &$record) {
			if ($options['deep']) {
				$validates = $this->validateAssociated($record, $options);
			} else {
				$this->create(null);
				$validates = $this->set($record) && $this->validates($options);
				$data[$key] = $this->data;
			}
			if ($validates === false || (is_array($validates) && in_array(false, $validates, true))) {
				$validationErrors[$key] = $this->validationErrors;
				$validates = false;
			} else {
				$validates = true;
			}
			$return[$key] = $validates;
		}
		$this->validationErrors = $validationErrors;
		if (!$options['atomic']) {
			return $return;
		}
		if (empty($this->validationErrors)) {
			return true;
		}
		return false;
	}

/**
 * Saves a single record, as well as all its directly associated records.
 *
 * #### Options
 *
 * - `validate` Set to `false` to disable validation, `true` to validate each record before saving,
 *   'first' to validate *all* records before any are saved(default),
 * - `atomic` If true (default), will attempt to save all records in a single transaction.
 *   Should be set to false if database/table does not support transactions.
 * - fieldList: Equivalent to the $fieldList parameter in Model::save().
 *   It should be an associate array with model name as key and array of fields as value. Eg.
 *   {{{
 *   array(
 *       'SomeModel' => array('field'),
 *       'AssociatedModel' => array('field', 'otherfield')
 *   )
 *   }}}
 * - deep: If set to true, not only directly associated data is saved, but deeper nested associated data as well.
 *
 * @param array $data Record data to save. This should be an array indexed by association name.
 * @param array $options Options to use when saving record data, See $options above.
 * @return mixed If atomic: True on success, or false on failure.
 *    Otherwise: array similar to the $data array passed, but values are set to true/false
 *    depending on whether each record saved successfully.
 * @link http://book.cakephp.org/2.0/en/models/saving-your-data.html#model-saveassociated-array-data-null-array-options-array
 */
	public function saveAssociated($data = null, $options = array()) {
		if (empty($data)) {
			$data = $this->data;
		}

		$options = array_merge(array('validate' => 'first', 'atomic' => true, 'deep' => false), $options);
		$this->validationErrors = $validationErrors = array();

		if (empty($data) && $options['validate'] !== false) {
			$result = $this->save($data, $options);
			if (!$options['atomic']) {
				return array(!empty($result));
			}
			return !empty($result);
		}

		if ($options['validate'] === 'first') {
			$validates = $this->validateAssociated($data, $options);
			if ((!$validates && $options['atomic']) || (!$options['atomic'] && in_array(false, $validates, true))) {
				return $validates;
			}
			$options['validate'] = false;
		}
		if ($options['atomic']) {
			$db = $this->getDataSource();
			$transactionBegun = $db->begin();
		}

		$associations = $this->getAssociated();
		$return = array();
		$validates = true;
		foreach ($data as $association => $values) {
			$notEmpty = !empty($values[$association]) || (!isset($values[$association]) && !empty($values));
			if (isset($associations[$association]) && $associations[$association] === 'belongsTo' && $notEmpty) {
				$validates = $this->{$association}->create(null) !== null;
				$saved = false;
				if ($validates) {
					if ($options['deep']) {
						$saved = $this->{$association}->saveAssociated($values, array_merge($options, array('atomic' => false)));
					} else {
						$saved = $this->{$association}->save($values, array_merge($options, array('atomic' => false)));
					}
					$validates = ($saved === true || (is_array($saved) && !in_array(false, $saved, true)));
				}
				if ($validates) {
					$key = $this->belongsTo[$association]['foreignKey'];
					if (isset($data[$this->alias])) {
						$data[$this->alias][$key] = $this->{$association}->id;
					} else {
						$data = array_merge(array($key => $this->{$association}->id), $data, array($key => $this->{$association}->id));
					}
				} else {
					$validationErrors[$association] = $this->{$association}->validationErrors;
				}
				$return[$association] = $validates;
			}
		}
		if ($validates && !($this->create(null) !== null && $this->save($data, $options))) {
			$validationErrors[$this->alias] = $this->validationErrors;
			$validates = false;
		}
		$return[$this->alias] = $validates;

		foreach ($data as $association => $values) {
			if (!$validates) {
				break;
			}
			$notEmpty = !empty($values[$association]) || (!isset($values[$association]) && !empty($values));
			if (isset($associations[$association]) && $notEmpty) {
				$type = $associations[$association];
				$key = $this->{$type}[$association]['foreignKey'];
				switch ($type) {
					case 'hasOne':
						if (isset($values[$association])) {
							$values[$association][$key] = $this->id;
						} else {
							$values = array_merge(array($key => $this->id), $values, array($key => $this->id));
						}
						$validates = $this->{$association}->create(null) !== null;
						$saved = false;
						if ($validates) {
							if ($options['deep']) {
								$saved = $this->{$association}->saveAssociated($values, array_merge($options, array('atomic' => false)));
							} else {
								$saved = $this->{$association}->save($values, $options);
							}
						}
						$validates = ($validates && ($saved === true || (is_array($saved) && !in_array(false, $saved, true))));
						if (!$validates) {
							$validationErrors[$association] = $this->{$association}->validationErrors;
						}
						$return[$association] = $validates;
					break;
					case 'hasMany':
						foreach ($values as $i => $value) {
							if (isset($values[$i][$association])) {
								$values[$i][$association][$key] = $this->id;
							} else {
								$values[$i] = array_merge(array($key => $this->id), $value, array($key => $this->id));
							}
						}
						$_return = $this->{$association}->saveMany($values, array_merge($options, array('atomic' => false)));
						if (in_array(false, $_return, true)) {
							$validationErrors[$association] = $this->{$association}->validationErrors;
							$validates = false;
						}
						$return[$association] = $_return;
					break;
				}
			}
		}
		$this->validationErrors = $validationErrors;

		if (isset($validationErrors[$this->alias])) {
			$this->validationErrors = $validationErrors[$this->alias];
		}

		if (!$options['atomic']) {
			return $return;
		}
		if ($validates) {
			if ($transactionBegun) {
				return $db->commit() !== false;
			} else {
				return true;
			}
		}
		$db->rollback();
		return false;
	}

/**
 * Validates a single record, as well as all its directly associated records.
 *
 * #### Options
 *
 * - atomic: If true (default), returns boolean. If false returns array.
 * - fieldList: Equivalent to the $fieldList parameter in Model::save()
 * - deep: If set to true, not only directly associated data , but deeper nested associated data is validated as well.
 *
 * Warning: This method could potentially change the passed argument `$data`,
 * If you do not want this to happen, make a copy of `$data` before passing it
 * to this method
 *
 * @param array $data Record data to validate. This should be an array indexed by association name.
 * @param array $options Options to use when validating record data (see above), See also $options of validates().
 * @return array|boolean If atomic: True on success, or false on failure.
 *    Otherwise: array similar to the $data array passed, but values are set to true/false
 *    depending on whether each record validated successfully.
 */
	public function validateAssociated(&$data, $options = array()) {
		$options = array_merge(array('atomic' => true, 'deep' => false), $options);
		$this->validationErrors = $validationErrors = $return = array();
		$this->create(null);
		if (!($this->set($data) && $this->validates($options))) {
			$validationErrors[$this->alias] = $this->validationErrors;
			$return[$this->alias] = false;
		} else {
			$return[$this->alias] = true;
		}
		$data = $this->data;
		if (!empty($options['deep']) && isset($data[$this->alias])) {
			$recordData = $data[$this->alias];
			unset($data[$this->alias]);
			$data = array_merge($data, $recordData);
		}

		$associations = $this->getAssociated();
		foreach ($data as $association => &$values) {
			$validates = true;
			if (isset($associations[$association])) {
				if (in_array($associations[$association], array('belongsTo', 'hasOne'))) {
					if ($options['deep']) {
						$validates = $this->{$association}->validateAssociated($values, $options);
					} else {
						$validates = $this->{$association}->create($values) !== null && $this->{$association}->validates($options);
					}
					if (is_array($validates)) {
						if (in_array(false, $validates, true)) {
							$validates = false;
						} else {
							$validates = true;
						}
					}
					$return[$association] = $validates;
				} elseif ($associations[$association] === 'hasMany') {
					$validates = $this->{$association}->validateMany($values, $options);
					$return[$association] = $validates;
				}
				if (!$validates || (is_array($validates) && in_array(false, $validates, true))) {
					$validationErrors[$association] = $this->{$association}->validationErrors;
				}
			}
		}

		$this->validationErrors = $validationErrors;
		if (isset($validationErrors[$this->alias])) {
			$this->validationErrors = $validationErrors[$this->alias];
		}
		if (!$options['atomic']) {
			return $return;
		}
		if ($return[$this->alias] === false || !empty($this->validationErrors)) {
			return false;
		}
		return true;
	}

/**
 * Updates multiple model records based on a set of conditions.
 *
 * @param array $fields Set of fields and values, indexed by fields.
 *    Fields are treated as SQL snippets, to insert literal values manually escape your data.
 * @param mixed $conditions Conditions to match, true for all records
 * @return boolean True on success, false on failure
 * @link http://book.cakephp.org/2.0/en/models/saving-your-data.html#model-updateall-array-fields-array-conditions
 */
	public function updateAll($fields, $conditions = true) {
		return $this->getDataSource()->update($this, $fields, null, $conditions);
	}

/**
 * Removes record for given ID. If no ID is given, the current ID is used. Returns true on success.
 *
 * @param mixed $id ID of record to delete
 * @param boolean $cascade Set to true to delete records that depend on this record
 * @return boolean True on success
 * @link http://book.cakephp.org/2.0/en/models/deleting-data.html
 */
	public function delete($id = null, $cascade = true) {
		if (!empty($id)) {
			$this->id = $id;
		}
		$id = $this->id;

		$event = new CakeEvent('Model.beforeDelete', $this, array($cascade));
		list($event->break, $event->breakOn) = array(true, array(false, null));
		$this->getEventManager()->dispatch($event);
		if (!$event->isStopped()) {
			if (!$this->exists()) {
				return false;
			}
			$db = $this->getDataSource();

			$this->_deleteDependent($id, $cascade);
			$this->_deleteLinks($id);
			$this->id = $id;

			$updateCounterCache = false;
			if (!empty($this->belongsTo)) {
				foreach ($this->belongsTo as $parent => $assoc) {
					if (!empty($assoc['counterCache'])) {
						$updateCounterCache = true;
						break;
					}
				}

				$keys = $this->find('first', array(
					'fields' => $this->_collectForeignKeys(),
					'conditions' => array($this->alias . '.' . $this->primaryKey => $id),
					'recursive' => -1,
					'callbacks' => false
				));
			}

			if ($db->delete($this, array($this->alias . '.' . $this->primaryKey => $id))) {
				if ($updateCounterCache) {
					$this->updateCounterCache($keys[$this->alias]);
				}
				$this->getEventManager()->dispatch(new CakeEvent('Model.afterDelete', $this));
				$this->_clearCache();
				$this->id = false;
				return true;
			}
		}
		return false;
	}

/**
 * Cascades model deletes through associated hasMany and hasOne child records.
 *
 * @param string $id ID of record that was deleted
 * @param boolean $cascade Set to true to delete records that depend on this record
 * @return void
 */
	protected function _deleteDependent($id, $cascade) {
		if (!empty($this->__backAssociation)) {
			$savedAssociatons = $this->__backAssociation;
			$this->__backAssociation = array();
		}
		if ($cascade === true) {
			foreach (array_merge($this->hasMany, $this->hasOne) as $assoc => $data) {
				if ($data['dependent'] === true) {

					$model = $this->{$assoc};

					if ($data['foreignKey'] === false && $data['conditions'] && in_array($this->name, $model->getAssociated('belongsTo'))) {
						$model->recursive = 0;
						$conditions = array($this->escapeField(null, $this->name) => $id);
					} else {
						$model->recursive = -1;
						$conditions = array($model->escapeField($data['foreignKey']) => $id);
						if ($data['conditions']) {
							$conditions = array_merge((array)$data['conditions'], $conditions);
						}
					}

					if (isset($data['exclusive']) && $data['exclusive']) {
						$model->deleteAll($conditions);
					} else {
						$records = $model->find('all', array(
							'conditions' => $conditions, 'fields' => $model->primaryKey
						));

						if (!empty($records)) {
							foreach ($records as $record) {
								$model->delete($record[$model->alias][$model->primaryKey]);
							}
						}
					}
				}
			}
		}
		if (isset($savedAssociatons)) {
			$this->__backAssociation = $savedAssociatons;
		}
	}

/**
 * Cascades model deletes through HABTM join keys.
 *
 * @param string $id ID of record that was deleted
 * @return void
 */
	protected function _deleteLinks($id) {
		foreach ($this->hasAndBelongsToMany as $assoc => $data) {
			list($plugin, $joinModel) = pluginSplit($data['with']);
			$records = $this->{$joinModel}->find('all', array(
				'conditions' => array($this->{$joinModel}->escapeField($data['foreignKey']) => $id),
				'fields' => $this->{$joinModel}->primaryKey,
				'recursive' => -1,
				'callbacks' => false
			));
			if (!empty($records)) {
				foreach ($records as $record) {
					$this->{$joinModel}->delete($record[$this->{$joinModel}->alias][$this->{$joinModel}->primaryKey]);
				}
			}
		}
	}

/**
 * Deletes multiple model records based on a set of conditions.
 *
 * @param mixed $conditions Conditions to match
 * @param boolean $cascade Set to true to delete records that depend on this record
 * @param boolean $callbacks Run callbacks
 * @return boolean True on success, false on failure
 * @link http://book.cakephp.org/2.0/en/models/deleting-data.html#deleteall
 */
	public function deleteAll($conditions, $cascade = true, $callbacks = false) {
		if (empty($conditions)) {
			return false;
		}
		$db = $this->getDataSource();

		if (!$cascade && !$callbacks) {
			return $db->delete($this, $conditions);
		} else {
			$ids = $this->find('all', array_merge(array(
				'fields' => "{$this->alias}.{$this->primaryKey}",
				'recursive' => 0), compact('conditions'))
			);
			if ($ids === false) {
				return false;
			}

			$ids = Set::extract($ids, "{n}.{$this->alias}.{$this->primaryKey}");
			if (empty($ids)) {
				return true;
			}

			if ($callbacks) {
				$_id = $this->id;
				$result = true;
				foreach ($ids as $id) {
					$result = ($result && $this->delete($id, $cascade));
				}
				$this->id = $_id;
				return $result;
			} else {
				foreach ($ids as $id) {
					$this->_deleteLinks($id);
					if ($cascade) {
						$this->_deleteDependent($id, $cascade);
					}
				}
				return $db->delete($this, array($this->alias . '.' . $this->primaryKey => $ids));
			}
		}
	}

/**
 * Collects foreign keys from associations.
 *
 * @param string $type
 * @return array
 */
	protected function _collectForeignKeys($type = 'belongsTo') {
		$result = array();

		foreach ($this->{$type} as $assoc => $data) {
			if (isset($data['foreignKey']) && is_string($data['foreignKey'])) {
				$result[$assoc] = $data['foreignKey'];
			}
		}
		return $result;
	}

/**
 * Returns true if a record with particular ID exists.
 *
 * If $id is not passed it calls Model::getID() to obtain the current record ID,
 * and then performs a Model::find('count') on the currently configured datasource
 * to ascertain the existence of the record in persistent storage.
 *
 * @param mixed $id ID of record to check for existence
 * @return boolean True if such a record exists
 */
	public function exists($id = null) {
		if ($id === null) {
			$id = $this->getID();
		}
		if ($id === false) {
			return false;
		}
		$conditions = array($this->alias . '.' . $this->primaryKey => $id);
		$query = array('conditions' => $conditions, 'recursive' => -1, 'callbacks' => false);
		return ($this->find('count', $query) > 0);
	}

/**
 * Returns true if a record that meets given conditions exists.
 *
 * @param array $conditions SQL conditions array
 * @return boolean True if such a record exists
 */
	public function hasAny($conditions = null) {
		return ($this->find('count', array('conditions' => $conditions, 'recursive' => -1)) != false);
	}

/**
 * Queries the datasource and returns a result set array.
 *
 * Also used to perform notation finds, where the first argument is type of find operation to perform
 * (all / first / count / neighbors / list / threaded),
 * second parameter options for finding ( indexed array, including: 'conditions', 'limit',
 * 'recursive', 'page', 'fields', 'offset', 'order')
 *
 * Eg:
 * {{{
 * 	find('all', array(
 * 		'conditions' => array('name' => 'Thomas Anderson'),
 * 		'fields' => array('name', 'email'),
 * 		'order' => 'field3 DESC',
 * 		'recursive' => 2,
 * 		'group' => 'type'
 * ));
 * }}}
 *
 * In addition to the standard query keys above, you can provide Datasource, and behavior specific
 * keys.  For example, when using a SQL based datasource you can use the joins key to specify additional
 * joins that should be part of the query.
 *
 * {{{
 * find('all', array(
 * 		'conditions' => array('name' => 'Thomas Anderson'),
 * 		'joins' => array(
 *			array(
 * 				'alias' => 'Thought',
 * 				'table' => 'thoughts',
 * 				'type' => 'LEFT',
 * 				'conditions' => '`Thought`.`person_id` = `Person`.`id`'
 *			)
 * 		)
 * ));
 * }}}
 *
 * Behaviors and find types can also define custom finder keys which are passed into find().
 *
 * Specifying 'fields' for notation 'list':
 *
 *  - If no fields are specified, then 'id' is used for key and 'model->displayField' is used for value.
 *  - If a single field is specified, 'id' is used for key and specified field is used for value.
 *  - If three fields are specified, they are used (in order) for key, value and group.
 *  - Otherwise, first and second fields are used for key and value.
 *
 *  Note: find(list) + database views have issues with MySQL 5.0. Try upgrading to MySQL 5.1 if you
 *  have issues with database views.
 * @param string $type Type of find operation (all / first / count / neighbors / list / threaded)
 * @param array $query Option fields (conditions / fields / joins / limit / offset / order / page / group / callbacks)
 * @return array Array of records
 * @link http://book.cakephp.org/2.0/en/models/deleting-data.html#deleteall
 */
	public function find($type = 'first', $query = array()) {
		$this->findQueryType = $type;
		$this->id = $this->getID();

		$query = $this->buildQuery($type, $query);
		if (is_null($query)) {
			return null;
		}

		$results = $this->getDataSource()->read($this, $query);
		$this->resetAssociations();

		if ($query['callbacks'] === true || $query['callbacks'] === 'after') {
			$results = $this->_filterResults($results);
		}

		$this->findQueryType = null;

		if ($type === 'all') {
			return $results;
		} else {
			if ($this->findMethods[$type] === true) {
				return $this->{'_find' . ucfirst($type)}('after', $query, $results);
			}
		}
	}

/**
 * Builds the query array that is used by the data source to generate the query to fetch the data.
 *
 * @param string $type Type of find operation (all / first / count / neighbors / list / threaded)
 * @param array $query Option fields (conditions / fields / joins / limit / offset / order / page / group / callbacks)
 * @return array Query array or null if it could not be build for some reasons
 * @see Model::find()
 */
	public function buildQuery($type = 'first', $query = array()) {
		$query = array_merge(
			array(
				'conditions' => null, 'fields' => null, 'joins' => array(), 'limit' => null,
				'offset' => null, 'order' => null, 'page' => 1, 'group' => null, 'callbacks' => true,
			),
			(array)$query
		);

		if ($type !== 'all') {
			if ($this->findMethods[$type] === true) {
				$query = $this->{'_find' . ucfirst($type)}('before', $query);
			}
		}

		if (!is_numeric($query['page']) || intval($query['page']) < 1) {
			$query['page'] = 1;
		}
		if ($query['page'] > 1 && !empty($query['limit'])) {
			$query['offset'] = ($query['page'] - 1) * $query['limit'];
		}
		if ($query['order'] === null && $this->order !== null) {
			$query['order'] = $this->order;
		}
		$query['order'] = array($query['order']);

		if ($query['callbacks'] === true || $query['callbacks'] === 'before') {
			$event = new CakeEvent('Model.beforeFind', $this, array($query));
			list($event->break, $event->breakOn, $event->modParams) = array(true, array(false, null), 0);
			$this->getEventManager()->dispatch($event);
			if ($event->isStopped()) {
				return null;
			}
			$query = $event->result === true ? $event->data[0] : $event->result;
		}

		return $query;
	}

/**
 * Handles the before/after filter logic for find('first') operations.  Only called by Model::find().
 *
 * @param string $state Either "before" or "after"
 * @param array $query
 * @param array $results
 * @return array
 * @see Model::find()
 */
	protected function _findFirst($state, $query, $results = array()) {
		if ($state === 'before') {
			$query['limit'] = 1;
			return $query;
		} elseif ($state === 'after') {
			if (empty($results[0])) {
				return false;
			}
			return $results[0];
		}
	}

/**
 * Handles the before/after filter logic for find('count') operations.  Only called by Model::find().
 *
 * @param string $state Either "before" or "after"
 * @param array $query
 * @param array $results
 * @return integer The number of records found, or false
 * @see Model::find()
 */
	protected function _findCount($state, $query, $results = array()) {
		if ($state === 'before') {
			$db = $this->getDataSource();
			$query['order'] = false;
			if (!method_exists($db, 'calculate')) {
				return $query;
			}
			if (empty($query['fields'])) {
				$query['fields'] = $db->calculate($this, 'count');
			} elseif (method_exists($db, 'expression') && is_string($query['fields']) && !preg_match('/count/i', $query['fields'])) {
				$query['fields'] = $db->calculate($this, 'count', array(
					$db->expression($query['fields']), 'count'
				));
			}
			return $query;
		} elseif ($state === 'after') {
			foreach (array(0, $this->alias) as $key) {
				if (isset($results[0][$key]['count'])) {
					if (($count = count($results)) > 1) {
						return $count;
					} else {
						return intval($results[0][$key]['count']);
					}
				}
			}
			return false;
		}
	}

/**
 * Handles the before/after filter logic for find('list') operations.  Only called by Model::find().
 *
 * @param string $state Either "before" or "after"
 * @param array $query
 * @param array $results
 * @return array Key/value pairs of primary keys/display field values of all records found
 * @see Model::find()
 */
	protected function _findList($state, $query, $results = array()) {
		if ($state === 'before') {
			if (empty($query['fields'])) {
				$query['fields'] = array("{$this->alias}.{$this->primaryKey}", "{$this->alias}.{$this->displayField}");
				$list = array("{n}.{$this->alias}.{$this->primaryKey}", "{n}.{$this->alias}.{$this->displayField}", null);
			} else {
				if (!is_array($query['fields'])) {
					$query['fields'] = String::tokenize($query['fields']);
				}

				if (count($query['fields']) === 1) {
					if (strpos($query['fields'][0], '.') === false) {
						$query['fields'][0] = $this->alias . '.' . $query['fields'][0];
					}

					$list = array("{n}.{$this->alias}.{$this->primaryKey}", '{n}.' . $query['fields'][0], null);
					$query['fields'] = array("{$this->alias}.{$this->primaryKey}", $query['fields'][0]);
				} elseif (count($query['fields']) === 3) {
					for ($i = 0; $i < 3; $i++) {
						if (strpos($query['fields'][$i], '.') === false) {
							$query['fields'][$i] = $this->alias . '.' . $query['fields'][$i];
						}
					}

					$list = array('{n}.' . $query['fields'][0], '{n}.' . $query['fields'][1], '{n}.' . $query['fields'][2]);
				} else {
					for ($i = 0; $i < 2; $i++) {
						if (strpos($query['fields'][$i], '.') === false) {
							$query['fields'][$i] = $this->alias . '.' . $query['fields'][$i];
						}
					}

					$list = array('{n}.' . $query['fields'][0], '{n}.' . $query['fields'][1], null);
				}
			}
			if (!isset($query['recursive']) || $query['recursive'] === null) {
				$query['recursive'] = -1;
			}
			list($query['list']['keyPath'], $query['list']['valuePath'], $query['list']['groupPath']) = $list;
			return $query;
		} elseif ($state === 'after') {
			if (empty($results)) {
				return array();
			}
			$lst = $query['list'];
			return Set::combine($results, $lst['keyPath'], $lst['valuePath'], $lst['groupPath']);
		}
	}

/**
 * Detects the previous field's value, then uses logic to find the 'wrapping'
 * rows and return them.
 *
 * @param string $state Either "before" or "after"
 * @param mixed $query
 * @param array $results
 * @return array
 */
	protected function _findNeighbors($state, $query, $results = array()) {
		if ($state === 'before') {
			extract($query);
			$conditions = (array)$conditions;
			if (isset($field) && isset($value)) {
				if (strpos($field, '.') === false) {
					$field = $this->alias . '.' . $field;
				}
			} else {
				$field = $this->alias . '.' . $this->primaryKey;
				$value = $this->id;
			}
			$query['conditions'] = array_merge($conditions, array($field . ' <' => $value));
			$query['order'] = $field . ' DESC';
			$query['limit'] = 1;
			$query['field'] = $field;
			$query['value'] = $value;
			return $query;
		} elseif ($state === 'after') {
			extract($query);
			unset($query['conditions'][$field . ' <']);
			$return = array();
			if (isset($results[0])) {
				$prevVal = Set::extract('/' . str_replace('.', '/', $field), $results[0]);
				$query['conditions'][$field . ' >='] = $prevVal[0];
				$query['conditions'][$field . ' !='] = $value;
				$query['limit'] = 2;
			} else {
				$return['prev'] = null;
				$query['conditions'][$field . ' >'] = $value;
				$query['limit'] = 1;
			}
			$query['order'] = $field . ' ASC';
			$return2 = $this->find('all', $query);
			if (!array_key_exists('prev', $return)) {
				$return['prev'] = $return2[0];
			}
			if (count($return2) === 2) {
				$return['next'] = $return2[1];
			} elseif (count($return2) === 1 && !$return['prev']) {
				$return['next'] = $return2[0];
			} else {
				$return['next'] = null;
			}
			return $return;
		}
	}

/**
 * In the event of ambiguous results returned (multiple top level results, with different parent_ids)
 * top level results with different parent_ids to the first result will be dropped
 *
 * @param mixed $state
 * @param mixed $query
 * @param array $results
 * @return array Threaded results
 */
	protected function _findThreaded($state, $query, $results = array()) {
		if ($state === 'before') {
			return $query;
		} elseif ($state === 'after') {
			$parent = 'parent_id';
			if (isset($query['parent'])) {
				$parent = $query['parent'];
			}
			return Set::nest($results, array(
				'idPath' => '/' . $this->alias . '/' . $this->primaryKey,
				'parentPath' => '/' . $this->alias . '/' . $parent
			));
		}
	}

/**
 * Passes query results through model and behavior afterFilter() methods.
 *
 * @param array $results Results to filter
 * @param boolean $primary If this is the primary model results (results from model where the find operation was performed)
 * @return array Set of filtered results
 */
	protected function _filterResults($results, $primary = true) {
		$event = new CakeEvent('Model.afterFind', $this, array($results, $primary));
		$event->modParams = 0;
		$this->getEventManager()->dispatch($event);
		return $event->result;
	}

/**
 * This resets the association arrays for the model back
 * to those originally defined in the model. Normally called at the end
 * of each call to Model::find()
 *
 * @return boolean Success
 */
	public function resetAssociations() {
		if (!empty($this->__backAssociation)) {
			foreach ($this->_associations as $type) {
				if (isset($this->__backAssociation[$type])) {
					$this->{$type} = $this->__backAssociation[$type];
				}
			}
			$this->__backAssociation = array();
		}

		foreach ($this->_associations as $type) {
			foreach ($this->{$type} as $key => $name) {
				if (property_exists($this, $key) && !empty($this->{$key}->__backAssociation)) {
					$this->{$key}->resetAssociations();
				}
			}
		}
		$this->__backAssociation = array();
		return true;
	}

/**
 * Returns false if any fields passed match any (by default, all if $or = false) of their matching values.
 *
 * @param array $fields Field/value pairs to search (if no values specified, they are pulled from $this->data)
 * @param boolean $or If false, all fields specified must match in order for a false return value
 * @return boolean False if any records matching any fields are found
 */
	public function isUnique($fields, $or = true) {
		if (!is_array($fields)) {
			$fields = func_get_args();
			if (is_bool($fields[count($fields) - 1])) {
				$or = $fields[count($fields) - 1];
				unset($fields[count($fields) - 1]);
			}
		}

		foreach ($fields as $field => $value) {
			if (is_numeric($field)) {
				unset($fields[$field]);

				$field = $value;
				if (isset($this->data[$this->alias][$field])) {
					$value = $this->data[$this->alias][$field];
				} else {
					$value = null;
				}
			}

			if (strpos($field, '.') === false) {
				unset($fields[$field]);
				$fields[$this->alias . '.' . $field] = $value;
			}
		}
		if ($or) {
			$fields = array('or' => $fields);
		}
		if (!empty($this->id)) {
			$fields[$this->alias . '.' . $this->primaryKey . ' !='] = $this->id;
		}
		return ($this->find('count', array('conditions' => $fields, 'recursive' => -1)) == 0);
	}

/**
 * Returns a resultset for a given SQL statement. Custom SQL queries should be performed with this method.
 *
 * @param string $sql,... SQL statement
 * @return mixed Resultset array or boolean indicating success / failure depending on the query executed
 * @link http://book.cakephp.org/2.0/en/models/retrieving-your-data.html#model-query
 */
	public function query($sql) {
		$params = func_get_args();
		$db = $this->getDataSource();
		return call_user_func_array(array(&$db, 'query'), $params);
	}

/**
 * Returns true if all fields pass validation. Will validate hasAndBelongsToMany associations
 * that use the 'with' key as well. Since _saveMulti is incapable of exiting a save operation.
 *
 * Will validate the currently set data.  Use Model::set() or Model::create() to set the active data.
 *
 * @param array $options An optional array of custom options to be made available in the beforeValidate callback
 * @return boolean True if there are no errors
 */
	public function validates($options = array()) {
		$errors = $this->invalidFields($options);
		if (empty($errors) && $errors !== false) {
			$errors = $this->_validateWithModels($options);
		}
		if (is_array($errors)) {
			return count($errors) === 0;
		}
		return $errors;
	}

/**
 * Returns an array of fields that have failed validation. On the current model.
 *
 * @param string $options An optional array of custom options to be made available in the beforeValidate callback
 * @return array Array of invalid fields
 * @see Model::validates()
 */
	public function invalidFields($options = array()) {
		$event = new CakeEvent('Model.beforeValidate', $this, array($options));
		list($event->break, $event->breakOn) = array(true, false);
		$this->getEventManager()->dispatch($event);
		if ($event->isStopped()) {
			return false;
		}

		if (!isset($this->validate) || empty($this->validate)) {
			return $this->validationErrors;
		}

		$data = $this->data;
		$methods = array_map('strtolower', get_class_methods($this));
		$behaviorMethods = array_keys($this->Behaviors->methods());

		if (isset($data[$this->alias])) {
			$data = $data[$this->alias];
		} elseif (!is_array($data)) {
			$data = array();
		}

		$exists = null;

		$_validate = $this->validate;
		$whitelist = $this->whitelist;

		if (!empty($options['fieldList'])) {
			if (!empty($options['fieldList'][$this->alias]) && is_array($options['fieldList'][$this->alias])) {
				$whitelist = $options['fieldList'][$this->alias];
			} else {
				$whitelist = $options['fieldList'];
			}
		}

		if (!empty($whitelist)) {
			$validate = array();
			foreach ((array)$whitelist as $f) {
				if (!empty($this->validate[$f])) {
					$validate[$f] = $this->validate[$f];
				}
			}
			$this->validate = $validate;
		}

		$validationDomain = $this->validationDomain;
		if (empty($validationDomain)) {
			$validationDomain = 'default';
		}

		foreach ($this->validate as $fieldName => $ruleSet) {
			if (!is_array($ruleSet) || (is_array($ruleSet) && isset($ruleSet['rule']))) {
				$ruleSet = array($ruleSet);
			}
			$default = array(
				'allowEmpty' => null,
				'required' => null,
				'rule' => 'blank',
				'last' => true,
				'on' => null
			);

			foreach ($ruleSet as $index => $validator) {
				if (!is_array($validator)) {
					$validator = array('rule' => $validator);
				}
				$validator = array_merge($default, $validator);

				if (!empty($validator['on']) || in_array($validator['required'], array('create', 'update'), true)) {
					if ($exists === null) {
						$exists = $this->exists();
					}
					if ($validator['on'] == 'create' && $exists || $validator['on'] == 'update' && !$exists) {
						continue;
					}
					if ($validator['required'] === 'create' && !$exists || $validator['required'] === 'update' && $exists) {
						$validator['required'] = true;
					}
				}

				$valid = true;
				$requiredFail = (
					(!isset($data[$fieldName]) && $validator['required'] === true) ||
					(
						isset($data[$fieldName]) && (empty($data[$fieldName]) &&
						!is_numeric($data[$fieldName])) && $validator['allowEmpty'] === false
					)
				);

				if (!$requiredFail && array_key_exists($fieldName, $data)) {
					if (empty($data[$fieldName]) && $data[$fieldName] != '0' && $validator['allowEmpty'] === true) {
						break;
					}
					if (is_array($validator['rule'])) {
						$rule = $validator['rule'][0];
						unset($validator['rule'][0]);
						$ruleParams = array_merge(array($data[$fieldName]), array_values($validator['rule']));
					} else {
						$rule = $validator['rule'];
						$ruleParams = array($data[$fieldName]);
					}

					if (in_array(strtolower($rule), $methods)) {
						$ruleParams[] = $validator;
						$ruleParams[0] = array($fieldName => $ruleParams[0]);
						$valid = $this->dispatchMethod($rule, $ruleParams);
					} elseif (in_array($rule, $behaviorMethods) || in_array(strtolower($rule), $behaviorMethods)) {
						$ruleParams[] = $validator;
						$ruleParams[0] = array($fieldName => $ruleParams[0]);
						$valid = $this->Behaviors->dispatchMethod($this, $rule, $ruleParams);
					} elseif (method_exists('Validation', $rule)) {
						$valid = call_user_func_array(array('Validation', $rule), $ruleParams);
					} elseif (!is_array($validator['rule'])) {
						$valid = preg_match($rule, $data[$fieldName]);
					} elseif (Configure::read('debug') > 0) {
						trigger_error(__d('cake_dev', 'Could not find validation handler %s for %s', $rule, $fieldName), E_USER_WARNING);
					}
				}

				if ($requiredFail || !$valid || (is_string($valid) && strlen($valid) > 0)) {
					if (is_string($valid)) {
						$message = $valid;
					} elseif (isset($validator['message'])) {
						$args = null;
						if (is_array($validator['message'])) {
							$message = $validator['message'][0];
							$args = array_slice($validator['message'], 1);
						} else {
							$message = $validator['message'];
						}
						if (is_array($validator['rule']) && $args === null) {
							$args = array_slice($ruleSet[$index]['rule'], 1);
						}
						$message = __d($validationDomain, $message, $args);
					} elseif (is_string($index)) {
						if (is_array($validator['rule'])) {
							$args = array_slice($ruleSet[$index]['rule'], 1);
							$message = __d($validationDomain, $index, $args);
						} else {
							$message = __d($validationDomain, $index);
						}
					} elseif (!$requiredFail && is_numeric($index) && count($ruleSet) > 1) {
						$message = $index + 1;
					} else {
						$message = __d('cake_dev', 'This field cannot be left blank');
					}

					$this->invalidate($fieldName, $message);
					if ($validator['last']) {
						break;
					}
				}
			}
		}
		$this->validate = $_validate;
		return $this->validationErrors;
	}

/**
 * Runs validation for hasAndBelongsToMany associations that have 'with' keys
 * set. And data in the set() data set.
 *
 * @param array $options Array of options to use on Validation of with models
 * @return boolean Failure of validation on with models.
 * @see Model::validates()
 */
	protected function _validateWithModels($options) {
		$valid = true;
		foreach ($this->hasAndBelongsToMany as $assoc => $association) {
			if (empty($association['with']) || !isset($this->data[$assoc])) {
				continue;
			}
			list($join) = $this->joinModel($this->hasAndBelongsToMany[$assoc]['with']);
			$data = $this->data[$assoc];

			$newData = array();
			foreach ((array)$data as $row) {
				if (isset($row[$this->hasAndBelongsToMany[$assoc]['associationForeignKey']])) {
					$newData[] = $row;
				} elseif (isset($row[$join]) && isset($row[$join][$this->hasAndBelongsToMany[$assoc]['associationForeignKey']])) {
					$newData[] = $row[$join];
				}
			}
			if (empty($newData)) {
				continue;
			}
			foreach ($newData as $data) {
				$data[$this->hasAndBelongsToMany[$assoc]['foreignKey']] = $this->id;
				$this->{$join}->create($data);
				$valid = ($valid && $this->{$join}->validates($options));
			}
		}
		return $valid;
	}

/**
 * Marks a field as invalid, optionally setting the name of validation
 * rule (in case of multiple validation for field) that was broken.
 *
 * @param string $field The name of the field to invalidate
 * @param mixed $value Name of validation rule that was not failed, or validation message to
 *    be returned. If no validation key is provided, defaults to true.
 * @return void
 */
	public function invalidate($field, $value = true) {
		if (!is_array($this->validationErrors)) {
			$this->validationErrors = array();
		}
		$this->validationErrors[$field][] = $value;
	}

/**
 * Returns true if given field name is a foreign key in this model.
 *
 * @param string $field Returns true if the input string ends in "_id"
 * @return boolean True if the field is a foreign key listed in the belongsTo array.
 */
	public function isForeignKey($field) {
		$foreignKeys = array();
		if (!empty($this->belongsTo)) {
			foreach ($this->belongsTo as $assoc => $data) {
				$foreignKeys[] = $data['foreignKey'];
			}
		}
		return in_array($field, $foreignKeys);
	}

/**
 * Escapes the field name and prepends the model name. Escaping is done according to the
 * current database driver's rules.
 *
 * @param string $field Field to escape (e.g: id)
 * @param string $alias Alias for the model (e.g: Post)
 * @return string The name of the escaped field for this Model (i.e. id becomes `Post`.`id`).
 */
	public function escapeField($field = null, $alias = null) {
		if (empty($alias)) {
			$alias = $this->alias;
		}
		if (empty($field)) {
			$field = $this->primaryKey;
		}
		$db = $this->getDataSource();
		if (strpos($field, $db->name($alias) . '.') === 0) {
			return $field;
		}
		return $db->name($alias . '.' . $field);
	}

/**
 * Returns the current record's ID
 *
 * @param integer $list Index on which the composed ID is located
 * @return mixed The ID of the current record, false if no ID
 */
	public function getID($list = 0) {
		if (empty($this->id) || (is_array($this->id) && isset($this->id[0]) && empty($this->id[0]))) {
			return false;
		}

		if (!is_array($this->id)) {
			return $this->id;
		}

		if (isset($this->id[$list]) && !empty($this->id[$list])) {
			return $this->id[$list];
		} elseif (isset($this->id[$list])) {
			return false;
		}

		return current($this->id);
	}

/**
 * Returns the ID of the last record this model inserted.
 *
 * @return mixed Last inserted ID
 */
	public function getLastInsertID() {
		return $this->getInsertID();
	}

/**
 * Returns the ID of the last record this model inserted.
 *
 * @return mixed Last inserted ID
 */
	public function getInsertID() {
		return $this->_insertID;
	}

/**
 * Sets the ID of the last record this model inserted
 *
 * @param mixed $id Last inserted ID
 * @return void
 */
	public function setInsertID($id) {
		$this->_insertID = $id;
	}

/**
 * Returns the number of rows returned from the last query.
 *
 * @return integer Number of rows
 */
	public function getNumRows() {
		return $this->getDataSource()->lastNumRows();
	}

/**
 * Returns the number of rows affected by the last query.
 *
 * @return integer Number of rows
 */
	public function getAffectedRows() {
		return $this->getDataSource()->lastAffected();
	}

/**
 * Sets the DataSource to which this model is bound.
 *
 * @param string $dataSource The name of the DataSource, as defined in app/Config/database.php
 * @return void
 * @throws MissingConnectionException
 */
	public function setDataSource($dataSource = null) {
		$oldConfig = $this->useDbConfig;

		if ($dataSource != null) {
			$this->useDbConfig = $dataSource;
		}
		$db = ConnectionManager::getDataSource($this->useDbConfig);
		if (!empty($oldConfig) && isset($db->config['prefix'])) {
			$oldDb = ConnectionManager::getDataSource($oldConfig);

			if (!isset($this->tablePrefix) || (!isset($oldDb->config['prefix']) || $this->tablePrefix == $oldDb->config['prefix'])) {
				$this->tablePrefix = $db->config['prefix'];
			}
		} elseif (isset($db->config['prefix'])) {
			$this->tablePrefix = $db->config['prefix'];
		}

		$this->schemaName = $db->getSchemaName();

		if (empty($db) || !is_object($db)) {
			throw new MissingConnectionException(array('class' => $this->name));
		}
	}

/**
 * Gets the DataSource to which this model is bound.
 *
 * @return DataSource A DataSource object
 */
	public function getDataSource() {
		if (!$this->_sourceConfigured && $this->useTable !== false) {
			$this->_sourceConfigured = true;
			$this->setSource($this->useTable);
		}
		return ConnectionManager::getDataSource($this->useDbConfig);
	}

/**
 * Get associations
 *
 * @return array
 */
	public function associations() {
		return $this->_associations;
	}

/**
 * Gets all the models with which this model is associated.
 *
 * @param string $type Only result associations of this type
 * @return array Associations
 */
	public function getAssociated($type = null) {
		if ($type == null) {
			$associated = array();
			foreach ($this->_associations as $assoc) {
				if (!empty($this->{$assoc})) {
					$models = array_keys($this->{$assoc});
					foreach ($models as $m) {
						$associated[$m] = $assoc;
					}
				}
			}
			return $associated;
		} elseif (in_array($type, $this->_associations)) {
			if (empty($this->{$type})) {
				return array();
			}
			return array_keys($this->{$type});
		} else {
			$assoc = array_merge(
				$this->hasOne,
				$this->hasMany,
				$this->belongsTo,
				$this->hasAndBelongsToMany
			);
			if (array_key_exists($type, $assoc)) {
				foreach ($this->_associations as $a) {
					if (isset($this->{$a}[$type])) {
						$assoc[$type]['association'] = $a;
						break;
					}
				}
				return $assoc[$type];
			}
			return null;
		}
	}

/**
 * Gets the name and fields to be used by a join model.  This allows specifying join fields
 * in the association definition.
 *
 * @param string|array $assoc The model to be joined
 * @param array $keys Any join keys which must be merged with the keys queried
 * @return array
 */
	public function joinModel($assoc, $keys = array()) {
		if (is_string($assoc)) {
			list(, $assoc) = pluginSplit($assoc);
			return array($assoc, array_keys($this->{$assoc}->schema()));
		} elseif (is_array($assoc)) {
			$with = key($assoc);
			return array($with, array_unique(array_merge($assoc[$with], $keys)));
		}
		trigger_error(
			__d('cake_dev', 'Invalid join model settings in %s', $model->alias),
			E_USER_WARNING
		);
	}

/**
 * Called before each find operation. Return false if you want to halt the find
 * call, otherwise return the (modified) query data.
 *
 * @param array $queryData Data used to execute this query, i.e. conditions, order, etc.
 * @return mixed true if the operation should continue, false if it should abort; or, modified
 *               $queryData to continue with new $queryData
 * @link http://book.cakephp.org/2.0/en/models/callback-methods.html#beforefind
 */
	public function beforeFind($queryData) {
		return true;
	}

/**
 * Called after each find operation. Can be used to modify any results returned by find().
 * Return value should be the (modified) results.
 *
 * @param mixed $results The results of the find operation
 * @param boolean $primary Whether this model is being queried directly (vs. being queried as an association)
 * @return mixed Result of the find operation
 * @link http://book.cakephp.org/2.0/en/models/callback-methods.html#afterfind
 */
	public function afterFind($results, $primary = false) {
		return $results;
	}

/**
 * Called before each save operation, after validation. Return a non-true result
 * to halt the save.
 *
 * @param array $options
 * @return boolean True if the operation should continue, false if it should abort
 * @link http://book.cakephp.org/2.0/en/models/callback-methods.html#beforesave
 */
	public function beforeSave($options = array()) {
		return true;
	}

/**
 * Called after each successful save operation.
 *
 * @param boolean $created True if this save created a new record
 * @return void
 * @link http://book.cakephp.org/2.0/en/models/callback-methods.html#aftersave
 */
	public function afterSave($created) {
	}

/**
 * Called before every deletion operation.
 *
 * @param boolean $cascade If true records that depend on this record will also be deleted
 * @return boolean True if the operation should continue, false if it should abort
 * @link http://book.cakephp.org/2.0/en/models/callback-methods.html#beforedelete
 */
	public function beforeDelete($cascade = true) {
		return true;
	}

/**
 * Called after every deletion operation.
 *
 * @return void
 * @link http://book.cakephp.org/2.0/en/models/callback-methods.html#afterdelete
 */
	public function afterDelete() {
	}

/**
 * Called during validation operations, before validation. Please note that custom
 * validation rules can be defined in $validate.
 *
 * @param array $options Options passed from model::save(), see $options of model::save().
 * @return boolean True if validate operation should continue, false to abort
 * @link http://book.cakephp.org/2.0/en/models/callback-methods.html#beforevalidate
 */
	public function beforeValidate($options = array()) {
		return true;
	}

/**
 * Called when a DataSource-level error occurs.
 *
 * @return void
 * @link http://book.cakephp.org/2.0/en/models/callback-methods.html#onerror
 */
	public function onError() {
	}

/**
 * Clears cache for this model.
 *
 * @param string $type If null this deletes cached views if Cache.check is true
 *     Will be used to allow deleting query cache also
 * @return boolean true on delete
 */
	protected function _clearCache($type = null) {
		if ($type === null) {
			if (Configure::read('Cache.check') === true) {
				$assoc[] = strtolower(Inflector::pluralize($this->alias));
				$assoc[] = strtolower(Inflector::underscore(Inflector::pluralize($this->alias)));
				foreach ($this->_associations as $key => $association) {
					foreach ($this->$association as $key => $className) {
						$check = strtolower(Inflector::pluralize($className['className']));
						if (!in_array($check, $assoc)) {
							$assoc[] = strtolower(Inflector::pluralize($className['className']));
							$assoc[] = strtolower(Inflector::underscore(Inflector::pluralize($className['className'])));
						}
					}
				}
				clearCache($assoc);
				return true;
			}
		} else {
			//Will use for query cache deleting
		}
	}

}
