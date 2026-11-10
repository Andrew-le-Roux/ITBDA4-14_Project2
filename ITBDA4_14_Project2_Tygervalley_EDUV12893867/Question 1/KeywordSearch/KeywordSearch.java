import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.JobContext;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.FileSplit;
import org.apache.hadoop.mapreduce.lib.input.TextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;

public class KeywordSearch {

    static final String KEYWORD_KEY = "keyword.search";

    // 1.2 Provide a R or Java implementation of the Mapper for the above problem.
    public static class KeywordMapper extends Mapper<LongWritable, Text, Text, Text> {

        private Pattern keywordPattern;
        private final Text documentName = new Text();
        private final Text location = new Text();
        private long lineNumber = 0;

        @Override
        protected void setup(Context context) throws IOException {

            String keyword = context.getConfiguration().get(KEYWORD_KEY);

            keywordPattern = Pattern.compile(
                    "(?<![\\p{L}\\p{N}_])" + Pattern.quote(keyword) + "(?![\\p{L}\\p{N}_])",
                    Pattern.CASE_INSENSITIVE);

            documentName.set(((FileSplit) context.getInputSplit()).getPath().getName());
        }

        @Override
        protected void map(LongWritable offset, Text value, Context context)
                throws IOException, InterruptedException {
            lineNumber++;

            Matcher m = keywordPattern.matcher(value.toString());
            while (m.find()) {
                int column = m.start() + 1;
                location.set(lineNumber + "," + column);
                context.write(documentName, location);
            }
        }
    }

    // 1.3 Provide a R or Java implementation of the Reducer class
    public static class KeywordReducer extends Reducer<Text, Text, Text, Text> {

        @Override
        protected void reduce(Text documentName, Iterable<Text> locations, Context context)
                throws IOException, InterruptedException {
            List<long[]> positions = new ArrayList<>();
            for (Text loc : locations) {
                String[] p = loc.toString().split(",");
                positions.add(new long[] { Long.parseLong(p[0]), Long.parseLong(p[1]) });
            }

            Collections.sort(positions, (a, b) -> a[0] != b[0]
                    ? Long.compare(a[0], b[0]) : Long.compare(a[1], b[1]));

            StringBuilder sb = new StringBuilder();
            sb.append(positions.size()).append(positions.size() == 1 ? " match:" : " matches:");
            for (long[] p : positions) {
                sb.append(" (row ").append(p[0]).append(", col ").append(p[1]).append(')');
            }
            context.write(documentName, new Text(sb.toString()));
        }
    }

    public static class WholeFileTextInputFormat extends TextInputFormat {
        @Override
        protected boolean isSplitable(JobContext context, Path file) {
            return false;
        }
    }

    // 1.4 Provide an implementation of the Hadoop MapReduce configuration settings
    public static void main(String[] args) throws Exception {

        String input  = "input";
        String output = "output";

        System.out.print("Enter the keyword: ");
        System.out.flush();
        String keyword = new BufferedReader(new InputStreamReader(System.in, StandardCharsets.UTF_8)).readLine();

        if (keyword == null || keyword.trim().isEmpty()) {
            System.err.println("No keyword entered.");
            System.exit(2);
        }
        keyword = keyword.trim();

        //Hadoop configuration settings
        Configuration conf = new Configuration();
        conf.set(KEYWORD_KEY, keyword);
        conf.set("fs.defaultFS", "file:///");
        conf.set("mapreduce.framework.name", "local");
        conf.set("mapreduce.output.textoutputformat.separator", "\t");

        Job job = Job.getInstance(conf, "Keyword search: " + keyword);
        job.setJarByClass(KeywordSearch.class);
        job.setMapperClass(KeywordMapper.class);
        job.setReducerClass(KeywordReducer.class);
        job.setNumReduceTasks(1);
        job.setInputFormatClass(WholeFileTextInputFormat.class);
        job.setOutputFormatClass(TextOutputFormat.class);
        job.setMapOutputKeyClass(Text.class);
        job.setMapOutputValueClass(Text.class);
        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(Text.class);

        Path inPath = new Path(input);
        Path outPath = new Path(output);
        FileInputFormat.addInputPath(job, inPath);
        FileOutputFormat.setOutputPath(job, outPath);

        FileSystem fs = outPath.getFileSystem(conf);
        if (fs.exists(outPath)) {
            fs.delete(outPath, true);
        }

        boolean ok = job.waitForCompletion(true);
        if (!ok) {
            System.exit(1);
        }
    }
}