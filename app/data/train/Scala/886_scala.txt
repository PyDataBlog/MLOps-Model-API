package at.fh.swengb.resifoAndroid.activities.meldezettelEdit

import java.util.Calendar

import android.app.{DatePickerDialog, Dialog}
import android.content.{DialogInterface, Intent}
import android.graphics.{Color, PorterDuff}
import android.os.Bundle
import android.support.v7.app.{AlertDialog, AppCompatActivity}
import android.view.View
import android.view.View.OnClickListener
import android.widget._
import at.fh.swengb.resifoAndroid.R
import at.fh.swengb.resifoAndroid.activities.help.HelpActivity
import at.fh.swengb.resifoAndroid.db.DBHelper

/**
  * Created by laszlobalo on 02.01.17.
  */
class Meldezettel02a extends AppCompatActivity {

  val db = new DBHelper(this)
  val datePicker: DatePicker = null
  var calendar: Calendar = null
  var dateView: TextView = null
  var year: Int = 0
  var month: Int = 0
  var day: Int = 0
  var currentDate: String = ""

  override protected def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.meldezettel02a)
    dateView = findViewById(R.id.textView3).asInstanceOf[TextView]
    calendar = Calendar.getInstance
    year = calendar.get(Calendar.YEAR)
    month = calendar.get(Calendar.MONTH)
    day = calendar.get(Calendar.DAY_OF_MONTH)
    showDate(year, month + 1, day)
    currentDate = ""

    val nextButton: ImageView = findViewById(R.id.nxtButton).asInstanceOf[ImageView]
    val helpButton: Button = findViewById(R.id.buttonHilfe).asInstanceOf[Button]
    val importantB2: EditText = findViewById(R.id.editText4).asInstanceOf[EditText]
    val importantB1: EditText = findViewById(R.id.editText5).asInstanceOf[EditText]
    val importantB3: EditText = findViewById(R.id.editText6).asInstanceOf[EditText]
    val importantB4: EditText = findViewById(R.id.editText8).asInstanceOf[EditText]

    val activity1Button: Button = findViewById(R.id.button1).asInstanceOf[Button]
    val activity2Button: Button = findViewById(R.id.button2).asInstanceOf[Button]
    val activity3Button: Button = findViewById(R.id.button3).asInstanceOf[Button]
    val activity4Button: Button = findViewById(R.id.button4).asInstanceOf[Button]
    val activity5Button: Button = findViewById(R.id.button5).asInstanceOf[Button]
    val activity6Button: Button = findViewById(R.id.button6).asInstanceOf[Button]
    val activity7Button: Button = findViewById(R.id.button7).asInstanceOf[Button]
    val activity8Button: Button = findViewById(R.id.button98).asInstanceOf[Button]

    importantB1.getBackground.setColorFilter(Color.RED, PorterDuff.Mode.SRC_IN)
    importantB2.getBackground.setColorFilter(Color.RED, PorterDuff.Mode.SRC_IN)
    importantB3.getBackground.setColorFilter(Color.RED, PorterDuff.Mode.SRC_IN)
    importantB4.getBackground.setColorFilter(Color.RED, PorterDuff.Mode.SRC_IN)

    def timeChanged: Boolean = {
      if (currentDate == "") {
        Toast.makeText(getApplicationContext, "Sie haben das Datum noch nicht geändert!", Toast.LENGTH_SHORT).show()
        false
      }
      else true

    }

    def importantFill: Boolean = {
      if ((importantB1.getText.toString.trim == "" || importantB2.getText.toString.trim == "") || (importantB3.getText.toString.trim == "" || importantB4.getText.toString.trim == "")) {
        Toast.makeText(getApplicationContext, "Pflichtpfeld ausfüllen!", Toast
          .LENGTH_SHORT).show()
        false
      } else true
    }

    activity1Button.setOnClickListener(new OnClickListener {
      def onClick(v: View): Unit = {
        new AlertDialog.Builder(Meldezettel02a.this)
          .setMessage("Wenn Sie die Seite verlassen werden die Daten der aktuellen Seite nicht gespeichert. Möchten Sie fortfahren?")
          .setNegativeButton("Nein", null)
          .setPositiveButton("Ja", new android.content.DialogInterface.OnClickListener() {
            override def onClick(dialog: DialogInterface, which: Int): Unit = {
              val intent: Intent = new Intent(Meldezettel02a.this, classOf[HelpActivity])
              intent.addFlags(Intent.FLAG_ACTIVITY_REORDER_TO_FRONT)
              startActivity(intent)
              dialog.dismiss()
            }
          })
          .show()
      }
    })

    activity2Button.setOnClickListener(new OnClickListener {
      def onClick(v: View): Unit = {
        new AlertDialog.Builder(Meldezettel02a.this)
          .setMessage("Möchten Sie keinen anderen Staat angeben?")
          .setNegativeButton("Doch", null)
          .setPositiveButton("Nein, möchte ich nicht!", new android.content.DialogInterface.OnClickListener() {
            override def onClick(dialog: DialogInterface, which: Int): Unit = {
              val intent: Intent = new Intent(Meldezettel02a.this, classOf[HelpActivity])
              intent.addFlags(Intent.FLAG_ACTIVITY_REORDER_TO_FRONT)
              startActivity(intent)
              dialog.dismiss()
            }
          })
          .show()
      }
    })

    activity3Button.setOnClickListener(new OnClickListener {
      def onClick(v: View): Unit = {
        new AlertDialog.Builder(Meldezettel02a.this)
          .setMessage("Wenn Sie die Seite verlassen werden die Daten der aktuellen Seite nicht gespeichert. Möchten Sie fortfahren?")
          .setNegativeButton("Nein", null)
          .setPositiveButton("Ja", new android.content.DialogInterface.OnClickListener() {
            override def onClick(dialog: DialogInterface, which: Int): Unit = {
              val intent: Intent = new Intent(Meldezettel02a.this, classOf[Meldezettel03])
              intent.addFlags(Intent.FLAG_ACTIVITY_REORDER_TO_FRONT)
              startActivity(intent)
              dialog.dismiss()
            }
          })
          .show()
      }
    })

    activity4Button.setOnClickListener(new OnClickListener {
      def onClick(v: View): Unit = {
        new AlertDialog.Builder(Meldezettel02a.this)
          .setMessage("Wenn Sie die Seite verlassen werden die Daten der aktuellen Seite nicht gespeichert. Möchten Sie fortfahren?")
          .setNegativeButton("Nein", null)
          .setPositiveButton("Ja", new android.content.DialogInterface.OnClickListener() {
            override def onClick(dialog: DialogInterface, which: Int): Unit = {
              val intent: Intent = new Intent(Meldezettel02a.this, classOf[Meldezettel04])
              intent.addFlags(Intent.FLAG_ACTIVITY_REORDER_TO_FRONT)
              startActivity(intent)
              dialog.dismiss()
            }
          })
          .show()
      }
    })

    activity5Button.setOnClickListener(new OnClickListener {
      def onClick(v: View): Unit = {
        new AlertDialog.Builder(Meldezettel02a.this)
          .setMessage("Wenn Sie die Seite verlassen werden die Daten der aktuellen Seite nicht gespeichert. Möchten Sie fortfahren?")
          .setNegativeButton("Nein", null)
          .setPositiveButton("Ja", new android.content.DialogInterface.OnClickListener() {
            override def onClick(dialog: DialogInterface, which: Int): Unit = {
              val intent: Intent = new Intent(Meldezettel02a.this, classOf[Meldezettel05])
              intent.addFlags(Intent.FLAG_ACTIVITY_REORDER_TO_FRONT)
              startActivity(intent)
              dialog.dismiss()
            }
          })
          .show()
      }
    })

    activity6Button.setOnClickListener(new OnClickListener {
      def onClick(v: View): Unit = {
        new AlertDialog.Builder(Meldezettel02a .this)
          .setMessage("Wenn Sie die Seite verlassen werden die Daten der aktuellen Seite nicht gespeichert. Möchten Sie fortfahren?")
          .setNegativeButton("Nein", null)
          .setPositiveButton("Ja", new android.content.DialogInterface.OnClickListener() {
            override def onClick(dialog: DialogInterface, which: Int): Unit = {
              val intent: Intent = new Intent(Meldezettel02a.this, classOf[Meldezettel06])
              intent.addFlags(Intent.FLAG_ACTIVITY_REORDER_TO_FRONT)
              startActivity(intent)
              dialog.dismiss()
            }
          })
          .show()      }
    })

    activity7Button.setOnClickListener(new OnClickListener {
      def onClick(v: View): Unit = {
        new AlertDialog.Builder(Meldezettel02a.this)
          .setMessage("Wenn Sie die Seite verlassen werden die Daten der aktuellen Seite nicht gespeichert. Möchten Sie fortfahren?")
          .setNegativeButton("Nein", null)
          .setPositiveButton("Ja", new android.content.DialogInterface.OnClickListener() {
            override def onClick(dialog: DialogInterface, which: Int): Unit = {
              startActivity(new Intent(getApplicationContext, classOf[Meldezettel07]))

            }
          })
          .show()
      }
    })

    activity8Button.setOnClickListener(new OnClickListener {
      def onClick(v: View): Unit = {
        startActivity(new Intent(Meldezettel02a.this, classOf[Meldezettel08]))
      }
    })



    helpButton.setOnClickListener(new OnClickListener {
      def onClick(v: View): Unit = {
        new AlertDialog.Builder(Meldezettel02a.this)
          .setMessage("Benötigen Sie Hilfe?")
          .setNegativeButton("Nein", null)
          .setPositiveButton("Ja", new android.content.DialogInterface.OnClickListener() {
            override def onClick(dialog: DialogInterface, which: Int): Unit = {
              val intent: Intent = new Intent(Meldezettel02a.this, classOf[HelpActivity])
              intent.addFlags(Intent.FLAG_ACTIVITY_REORDER_TO_FRONT)
              startActivity(intent)
              dialog.dismiss()
            }
          })
          .show()
      }
    })

    nextButton.setOnClickListener(new OnClickListener {
      def onClick(v: View): Unit = {
        if (importantFill && timeChanged) {
          db.updatePage2a(importantB2.getText.toString, importantB1.getText.toString, importantB3.getText.toString, dateView.getText.toString, importantB4.getText.toString,"1")
          startActivity(new Intent(getApplicationContext, classOf[Meldezettel03]).addFlags(Intent.FLAG_ACTIVITY_REORDER_TO_FRONT))
        }
      }
    })

  }


  override def onBackPressed() {
    startActivity(new Intent(Meldezettel02a.this, classOf[Meldezettel02]).addFlags(Intent.FLAG_ACTIVITY_REORDER_TO_FRONT))
  }

  @SuppressWarnings(Array("deprecation"))
  def setDate(view: View) {
    new DatePickerDialog(this, myDateListener, year, month, day).show()
    Toast.makeText(getApplicationContext, "Datum wählen", Toast.LENGTH_SHORT).show
  }

  private val myDateListener: DatePickerDialog.OnDateSetListener = new DatePickerDialog.OnDateSetListener() {
    def onDateSet(arg0: DatePicker, arg1: Int, arg2: Int, arg3: Int) {
      val newarg2 = arg2 + 1
      currentDate = s"$arg1/$newarg2/$arg3"
      showDate(arg1, arg2 + 1, arg3)
    }
  }

  private def showDate(year: Int, month: Int, day: Int) {
    dateView.setText(s"$day/$month/$year")
  }

}