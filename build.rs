use std::{
    env,
    fs::{read_dir, File},
    io::Write,
    path::PathBuf,
};

use joinery::{separators::Newline, JoinableIterator};
use lazy_format::lazy_format;
use nom::{bytes::complete::tag, character::complete::digit1, IResult, Parser};
use nom_supreme::ParserExt;

fn parse_day_filename(input: &str) -> IResult<&str, i32, ()> {
    tag("day")
        .precedes(digit1)
        .terminated(tag(".rs"))
        .parse_from_str()
        .all_consuming()
        .parse(input)
}

fn main() {
    let project_root = env::current_dir().expect("couldn't get working directory");
    let source_directory = project_root.join("src");

    println!("cargo:rerun-if-changed={}", source_directory.display());

    let items = read_dir(&source_directory).expect("couldn't open the source directory");

    let days: Vec<i32> = items
        .map(|item| item.expect("failed to read directory entry"))
        .filter(|item| item.file_type().unwrap().is_file())
        .filter_map(|item| {
            parse_day_filename(
                item.path()
                    .file_name()
                    .expect("file has no filename")
                    .to_str()
                    .expect("filename wasn't valid utf8"),
            )
            .ok()
            .map(|(_, day)| day)
        })
        .collect();

    let mods = days
        .iter()
        .map(|day| {
            // HATE HATE HATE HATE
            lazy_format!("#[path = \"../../../../../src/day{day}.rs\"] mod day{day};",)
        })
        .join_with(Newline);

    let enum_variants = days
        .iter()
        .map(|day| lazy_format!("Day{},", day))
        .join_with(Newline);

    let match_arms = days
        .iter()
        .map(|day| lazy_format!("{day} => Ok(Day::Day{day}),", day = day))
        .join_with(Newline);

    let solver_match_arms = days
        .iter()
        .flat_map(|&day| [(day, 1), (day, 2)])
        .map(|(day, part)| {
            lazy_format!(
                "#[allow(clippy::unnecessary_fallible_conversions)]
                #[allow(clippy::useless_conversion)]
                (Day::Day{day}, Part::Part{part}) => input
                    .try_into()
                    .map(|input| {{
                        if show_input {{
                            eprintln!(\"Parsed input:\n{{input:#?}}\");
                        }}
                        input
                    }})
                    .context(\"failed to parse input\")
                    .and_then(|input| day{day}::part{part}(input).context(\"failed to compute solution after successful parse\"))
                    .context(\"failed to solve day {day}, part {part}\")
                    .map(|solution| println!(\"{{solution}}\")),
                ",
            )
        })
        .join_with(Newline);

    let generated_content = lazy_format!(
        "
        {mods}

        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        enum Day {{
            {enum_variants}
        }}

        impl FromStr for Day {{
            type Err = DayError;

            fn from_str(s: &str) -> Result<Self, Self::Err> {{
                let value: u8 = s.parse()?;

                match value {{
                    {match_arms}
                    value => Err(DayError::BadDay(value)),
                }}
            }}
        }}


        fn run_solution(day: Day, part: Part, input: &str, show_input: bool) -> anyhow::Result<()> {{
            match (day, part) {{
                {solver_match_arms}
            }}
        }}"
    );

    let generated_content = generated_content.to_string();

    let output_path = PathBuf::from(env::var("OUT_DIR").expect("OUT_DIR not set in build.rs"));
    let output_path = output_path.join("generated.rs");

    let mut output = File::create(output_path).expect("failed to create generated.rs");
    output
        .write_all(generated_content.as_bytes())
        .expect("failed to write to generated.rs");

    output.flush().expect("failed to write to generated.rs");
}
