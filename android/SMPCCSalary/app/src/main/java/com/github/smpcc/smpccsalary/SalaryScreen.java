package com.github.smpcc.smpccsalary;

import android.app.ProgressDialog;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.PowerManager;
import android.preference.PreferenceManager;
import android.support.v7.app.ActionBarActivity;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import go.chat.Chat;
import go.Go;
import android.os.Handler;
import android.view.View;
import android.view.inputmethod.InputMethodManager;
import android.widget.EditText;
import android.widget.Spinner;
import android.widget.Switch;
import android.widget.TextView;

import com.jjoe64.graphview.DefaultLabelFormatter;
import com.jjoe64.graphview.GraphView;
import com.jjoe64.graphview.helper.StaticLabelsFormatter;
import com.jjoe64.graphview.series.BarGraphSeries;
import com.jjoe64.graphview.series.DataPoint;

public class SalaryScreen extends ActionBarActivity {

    EditText tf;
    TextView tv;
    Switch sw;
    GraphView gv;
    SharedPreferences preferences;
    ProgressDialog pd;

    PowerManager.WakeLock wl;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_salary_screen);
        Go.init(getApplicationContext());
        preferences = PreferenceManager.getDefaultSharedPreferences(getApplicationContext());
        PowerManager pm = (PowerManager) getSystemService(Context.POWER_SERVICE);
        wl = pm.newWakeLock(PowerManager.SCREEN_DIM_WAKE_LOCK, "smpcc");
        pd = new ProgressDialog(this);
        pd.setTitle("Please wait...");
        pd.setMessage("Running GMW");
        tf = (EditText) findViewById(R.id.salaryTextBox);
        tv = (TextView) findViewById(R.id.statusText);
        sw = (Switch) findViewById(R.id.genderSwitch);
        gv = (GraphView) findViewById(R.id.resultsGraph);
        StaticLabelsFormatter staticLabelsFormatter = new StaticLabelsFormatter(gv);
        staticLabelsFormatter.setHorizontalLabels(new String[]{"Women", "Men"});
        gv.getGridLabelRenderer().setLabelFormatter(staticLabelsFormatter);
    }

    public void buttonOnClick(View v) {
        Log.d("MPCC", "buttonOnClick entered");
        InputMethodManager imm = (InputMethodManager)getSystemService(
                Context.INPUT_METHOD_SERVICE);
        imm.hideSoftInputFromWindow(tf.getWindowToken(), 0);
        try {
            Log.d("MPCC", "Starting salary computation");
            Chat.StartSalaryComputation(sw.isChecked() ? 1 : 0, Integer.parseInt(tf.getText().toString()));
            pd.show();
            tv.setText("Computing...");
            wl.acquire();

        } catch (Exception e) {
            tv.append(e.getMessage());
        }
    }

    public void connectButtonOnClick(View v) {
        Chat.Init(preferences.getString("server_ip","127.0.0.1"));

        final Handler h = new Handler();
        Log.d("MPCC", "starting polling");
        h.postDelayed(new Runnable() {
            public void run() {
                Log.d("MPCC", "Calling poll function");
                String msg = Chat.PollForChanMessages();
                if (msg != null && msg.length() != 0) {
                    tv.setText("Status: computation completed. Result: " + msg);
                    pd.dismiss();
                    String[] parts = msg.split(" ");
                    int female = Integer.parseInt(parts[0]);
                    int male = Integer.parseInt(parts[1]);
                    BarGraphSeries<DataPoint> series = new BarGraphSeries<DataPoint>(new DataPoint[]{
                            new DataPoint(0, female),
                            new DataPoint(1, male),
                    });
                    gv.removeAllSeries();
                    gv.addSeries(series);
                    gv.getViewport().setMinY(0);
                    wl.release();
                }
                h.postDelayed(this, 1000);
            }
        }, 1000);
        tv.setText("Status: connection initiated...");
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.menu_salary_screen, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        // Handle action bar item clicks here. The action bar will
        // automatically handle clicks on the Home/Up button, so long
        // as you specify a parent activity in AndroidManifest.xml.
        int id = item.getItemId();

        //noinspection SimplifiableIfStatement
        if (id == R.id.action_settings) {
            Intent intent = new Intent(this, SettingsActivity.class);
            startActivity(intent);
            return true;
        }

        return super.onOptionsItemSelected(item);
    }
}
