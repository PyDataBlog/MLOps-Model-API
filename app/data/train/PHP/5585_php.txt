<?php

class CategoriasController extends Controller {

    public $layout = '//layouts/admin';

    public function actionIndex() {
        $CategoryData = Yii::app()->db->createCommand()->select(' * ')->from('categoria')->queryAll();
        $this->render('index', array('CategoryData' => $CategoryData));
    }

    public function actionForms() {

        $this->render('forms');
    }

    public function actionCreate() {
         FB::info($_POST, "____________________POST");

        $model = new Categoria;
        if(!empty($_POST)){
            $_POST['metas'] = implode(",", $_POST['metas']);
            $model->attributes = $_POST;
            $_POST['activo']=="true" ? $model->activo = 1 : $model->activo = 0;
            $model->fechaDeCreacion = new CDbExpression('NOW()');
            FB::INFO($model->attributes, '_______________________ATTRIBUTES');

            if ($model->save()){
                $storeFolder = getcwd() . '\\assets\\images\\categorias\\';
                FB::info($storeFolder, "storeFolder");
                if (!is_dir($storeFolder)) 
                    mkdir($storeFolder, 0777,true);
                $type = $_FILES['imagen']['type'];
                $extension = explode('/', $type);
                if (!empty($_FILES)) {
                    $tempFile = $_FILES['imagen']['tmp_name'];
                    $targetPath = $storeFolder;
                    $nuevoNombre = $model->idCategoria.'_'. $_POST['nuevoNombre'].'.'.$extension[1];
                    $targetFile = $targetPath . $nuevoNombre;
                    if (move_uploaded_file($tempFile, $targetFile)) {
                        FB::info("La imagen ha sido cargada al servidor.", "EXITO-Erasto");
                             $model->imagen = $nuevoNombre;
                        FB::info( $model->imagen, "modelo imagen");
                    }
                    else {
                            FB::info("No se guardo al imagen", "error erasto");
                    }
                }
                
                if ($model->save()) {
                    Yii::app()->user->setFlash("success", "La Categoria se Guardo Correctamente");
                }
            
                Yii::app()->user->setFlash("warning", "No se pudo guardar la categoria, por favor intente de nuevo.");
            }
        }
        $this->render('create');
    }

    public function actionUpdateajx() {
        FB::INFO($_POST, '__________________________POST');

        if ($_POST['Categoria'] && Yii::app()->request->isAjaxRequest) {
            $sql = "UPDATE categoria SET nombre = :nombre, posicion = :posicion WHERE idCategoria = :idCategoria";
            $parameters = array(
                ":idCategoria" => $_POST['Categoria']['idCategoria'],
                ':nombre' => $_POST['Categoria']['nombre'],
                ':posicion' => $_POST['Categoria']['posicion']
            );
            if (Yii::app()->db->createCommand($sql)->execute($parameters)) {

                $Content = $this->GetDataTableContent();

                echo CJSON::encode(array(
                    'requestresult' => 'ok',
                    'message' => "Datos Actualizados Correctamente...",
                    'content' => $Content
                ));
                return;
            } else {
                echo CJSON::encode(array(
                    'requestresult' => 'error',
                    'message' => "No se pudo Actualizar, intente de nuevo."
                ));
                return;
            }
        }
        return;
    }


    public function actionTest() {
        $CategoryData = Yii::app()->db->createCommand()->select(' * ')->from('categoria')->queryAll();
        $this->render('test', array('CategoryData' => $CategoryData));
    }


    public function actionDTable() {


        /* Array of database columns which should be read and sent back to DataTables. Use a space where
         * you want to insert a non-database field (for example a counter or static image)
         */
        $aColumns = array(
            'idCategoria',
            'nombre',
            'imagen',
            'posicion',
            'fechaDeCreacion'
        );

        /* Indexed column (used for fast and accurate table cardinality) */
        $sIndexColumn = "idCategoria";

        /* DB table to use */
        $sTable = "categoria";

        /* Database connection information */
        $gaSql['user'] = "root";
        $gaSql['password'] = "";
        $gaSql['db'] = "mesa_regalos";
        $gaSql['server'] = "127.0.0.1";

        /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         * If you just want to use the basic configuration for DataTables with PHP server-side, there is
         * no need to edit below this line
         */

        /*
         * MySQL connection
         */
        $gaSql['link'] = mysql_pconnect($gaSql['server'], $gaSql['user'], $gaSql['password']) or die('Could not open connection to server');

        mysql_select_db($gaSql['db'], $gaSql['link']) or die('Could not select database ' . $gaSql['db']);

        /*
         * Paging
         */
        $sLimit = "";
        if (isset($_GET['iDisplayStart']) && $_GET['iDisplayLength']!='-1') {
            $sLimit = "LIMIT " . mysql_real_escape_string($_GET['iDisplayStart']) . ", " . mysql_real_escape_string($_GET['iDisplayLength']);
        }

        /*
         * Ordering
         */
        if (isset($_GET['iSortCol_0'])) {
            $sOrder = "ORDER BY  ";
            for ($i = 0; $i<intval($_GET['iSortingCols']); $i++) {
                if ($_GET['bSortable_' . intval($_GET['iSortCol_' . $i])]=="true") {
                    $sOrder .= $aColumns[intval($_GET['iSortCol_' . $i])] . "
                    " . mysql_real_escape_string($_GET['sSortDir_' . $i]) . ", ";
                }
            }

            $sOrder = substr_replace($sOrder, "", -2);
            if ($sOrder=="ORDER BY") {
                $sOrder = "";
            }
        }

        /*
         * Filtering
         * NOTE this does not match the built-in DataTables filtering which does it
         * word by word on any field. It's possible to do here, but concerned about efficiency
         * on very large tables, and MySQL's regex functionality is very limited
         */
        $sWhere = "";
        if ($_GET['sSearch']!="") {
            $sWhere = "WHERE (";
            for ($i = 0; $i<count($aColumns); $i++) {
                $sWhere .= $aColumns[$i] . " LIKE '%" . mysql_real_escape_string($_GET['sSearch']) . "%' OR ";
            }
            $sWhere = substr_replace($sWhere, "", -3);
            $sWhere .= ')';
        }

        /* Individual column filtering */
        for ($i = 0; $i<count($aColumns); $i++) {
            if ($_GET['bSearchable_' . $i]=="true" && $_GET['sSearch_' . $i]!='') {
                if ($sWhere=="") {
                    $sWhere = "WHERE ";
                } else {
                    $sWhere .= " AND ";
                }
                $sWhere .= $aColumns[$i] . " LIKE '%" . mysql_real_escape_string($_GET['sSearch_' . $i]) . "%' ";
            }
        }

        /*
         * SQL queries
         * Get data to display
         */
        $sQuery = "
        SELECT SQL_CALC_FOUND_ROWS " . str_replace(" , ", " ", implode(", ", $aColumns)) . "
        FROM   $sTable
        $sWhere
        $sOrder
        $sLimit
    ";
        $rResult = mysql_query($sQuery, $gaSql['link']) or die(mysql_error());

        /* Data set length after filtering */
        $sQuery = "
        SELECT FOUND_ROWS()
    ";
        $rResultFilterTotal = mysql_query($sQuery, $gaSql['link']) or die(mysql_error());
        $aResultFilterTotal = mysql_fetch_array($rResultFilterTotal);
        $iFilteredTotal = $aResultFilterTotal[0];

        /* Total data set length */
        $sQuery = "
        SELECT COUNT(" . $sIndexColumn . ")
        FROM   $sTable
    ";
        $rResultTotal = mysql_query($sQuery, $gaSql['link']) or die(mysql_error());
        $aResultTotal = mysql_fetch_array($rResultTotal);
        $iTotal = $aResultTotal[0];

        /*
         * Output
         */
        $output = array(
            "sEcho" => intval($_GET['sEcho']),
            "iTotalRecords" => $iTotal,
            "iTotalDisplayRecords" => $iFilteredTotal,
            "aaData" => array()
        );

        while ($aRow = mysql_fetch_array($rResult)) {
            $row = array();
            for ($i = 0; $i<count($aColumns); $i++) {
                if ($aColumns[$i]=="version") {
                    /* Special output formatting for 'version' column */
                    $row[] = ($aRow[$aColumns[$i]]=="0") ? '-' : $aRow[$aColumns[$i]];
                } else if ($aColumns[$i]!=' ') {
                    /* General output */
                    $row[] = $aRow[$aColumns[$i]];
                }
            }
            $output['aaData'][] = $row;
        }

        echo json_encode($output);

    }

}
?>