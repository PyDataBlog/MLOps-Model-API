package eu.sii.pl;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 *
 */
public class SiiFrame extends JFrame {

    private JLabel loginLabel;
    private JTextField login;

    private JLabel passwordLabel;
    private JPasswordField password;

    private JButton okButton;

    public SiiFrame(String title) throws HeadlessException {
        super(title);

        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setDimensionAndPosition();

        buildContent();
    }

    private void setDimensionAndPosition() {
        setMinimumSize(new Dimension(400, 200));
        setLocationRelativeTo(null);
    }

    private void buildContent() {
        // Create components
        createComponents();

        // Configure components
        configureField(login);
        configureField(password);

        // Place components
        placeComponents();
    }

    private void placeComponents() {
        // Create panel
        GridBagLayout layout = new GridBagLayout();
        JPanel contentPane = new JPanel(layout);
        contentPane.setBackground(new Color(196, 196, 255));
        contentPane.setPreferredSize(new Dimension(300, 100));

        // Add components
        contentPane.add(loginLabel);
        contentPane.add(login);
        contentPane.add(passwordLabel);
        contentPane.add(password);
        contentPane.add(okButton);

        GridBagConstraints c = new GridBagConstraints();
        c.weightx = 1.0;

        c.fill = GridBagConstraints.BOTH;

        c.gridwidth = GridBagConstraints.RELATIVE;
        layout.setConstraints(loginLabel, c);
        layout.setConstraints(passwordLabel, c);

        c.gridwidth = GridBagConstraints.REMAINDER;
        c.weightx = 4.0;
        layout.setConstraints(login, c);
        layout.setConstraints(password, c);
        layout.setConstraints(okButton, c);

        getRootPane().setContentPane(contentPane);
    }

    private void createComponents() {
        // Login label
        loginLabel = new JLabel("Login");

        // Login field
        login = createLoginField();

        // Password label
        passwordLabel = new JLabel("Password");

        // Password field
        password = new JPasswordField();

        // OK button
        okButton = createOkButton();
    }

    private JButton createOkButton() {
        JButton button = new JButton("OK");
        button.setEnabled(false);
        button.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                JDialog dialog = new JDialog(SiiFrame.this, "Login Action", true);
                dialog.setLocationRelativeTo(SiiFrame.this);
                dialog.add(new Label("Hello " + login.getText()));
                dialog.setMinimumSize(new Dimension(300, 200));
                dialog.setVisible(true);
            }
        });
        return button;
    }

    private JTextField createLoginField() {
        JTextField field = new JTextField();
        field.getDocument().addDocumentListener(new DocumentListener() {
            @Override
            public void insertUpdate(DocumentEvent e) {
                validateInput();
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                validateInput();
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                validateInput();
            }
        });
        return field;
    }

    private void validateInput() {
        if (login.getText().length() <= 0) {
            okButton.setEnabled(false);
        } else {
            okButton.setEnabled(true);
        }
    }

    private void configureField(Component field) {
        Dimension size = field.getSize();
        size.setSize(100, size.getHeight());

        field.setMinimumSize(size);
    }
}
