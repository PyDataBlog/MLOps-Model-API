package sample;

import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.TextArea;
import javafx.scene.paint.Color;
import javafx.scene.text.Text;
import javafx.scene.text.TextFlow;

import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;

public class Controller implements Initializable {

    @FXML
    private TextArea input;
    @FXML
    private TextFlow display;

    public void FocusInput(){
        input.requestFocus();
    }

    @Override
    public void initialize(URL url, ResourceBundle resourceBundle) {

        Editor ed = new Editor();


        input.textProperty().addListener((observableValue, s, s2) -> {

            display.getChildren().clear();

            ed.render(observableValue.getValue());

            List x = ed.parse();

            x.forEach(i -> {

                List y = (List) i;
                Text t1 = new Text(y.get(1) + " ");
                if((int) y.get(0) == 1) t1.setFill(Color.BLUE);
                display.getChildren().add(t1);
                System.out.println(i);
            });


        });
    }
}
